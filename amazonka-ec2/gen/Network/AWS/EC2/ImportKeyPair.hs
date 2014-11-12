{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Imports the public key from an RSA key pair that you created with a
-- third-party tool. Compare this with CreateKeyPair, in which AWS creates the
-- key pair and gives the keys to you (AWS keeps a copy of the public key).
-- With ImportKeyPair, you create the key pair and give AWS just the public
-- key. The private key is never transferred between you and AWS. For more
-- information about key pairs, see Key Pairs in the Amazon Elastic Compute
-- Cloud User Guide.
module Network.AWS.EC2.ImportKeyPair
    (
    -- * Request
      ImportKeyPair
    -- ** Request constructor
    , importKeyPair
    -- ** Request lenses
    , ikpDryRun
    , ikpKeyName
    , ikpPublicKeyMaterial

    -- * Response
    , ImportKeyPairResult
    -- ** Response constructor
    , importKeyPairResult
    -- ** Response lenses
    , ikprKeyFingerprint
    , ikprKeyName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data ImportKeyPair = ImportKeyPair
    { _ikpDryRun            :: Maybe Bool
    , _ikpKeyName           :: Text
    , _ikpPublicKeyMaterial :: Base64
    } deriving (Eq, Show, Generic)

-- | 'ImportKeyPair' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ikpDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ikpKeyName' @::@ 'Text'
--
-- * 'ikpPublicKeyMaterial' @::@ 'Base64'
--
importKeyPair :: Text -- ^ 'ikpKeyName'
              -> Base64 -- ^ 'ikpPublicKeyMaterial'
              -> ImportKeyPair
importKeyPair p1 p2 = ImportKeyPair
    { _ikpKeyName           = p1
    , _ikpPublicKeyMaterial = p2
    , _ikpDryRun            = Nothing
    }

ikpDryRun :: Lens' ImportKeyPair (Maybe Bool)
ikpDryRun = lens _ikpDryRun (\s a -> s { _ikpDryRun = a })

-- | A unique name for the key pair.
ikpKeyName :: Lens' ImportKeyPair Text
ikpKeyName = lens _ikpKeyName (\s a -> s { _ikpKeyName = a })

-- | The public key. You must base64 encode the public key material before
-- sending it to AWS.
ikpPublicKeyMaterial :: Lens' ImportKeyPair Base64
ikpPublicKeyMaterial =
    lens _ikpPublicKeyMaterial (\s a -> s { _ikpPublicKeyMaterial = a })

instance ToQuery ImportKeyPair

instance ToPath ImportKeyPair where
    toPath = const "/"

data ImportKeyPairResult = ImportKeyPairResult
    { _ikprKeyFingerprint :: Maybe Text
    , _ikprKeyName        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ImportKeyPairResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ikprKeyFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'ikprKeyName' @::@ 'Maybe' 'Text'
--
importKeyPairResult :: ImportKeyPairResult
importKeyPairResult = ImportKeyPairResult
    { _ikprKeyName        = Nothing
    , _ikprKeyFingerprint = Nothing
    }

-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
ikprKeyFingerprint :: Lens' ImportKeyPairResult (Maybe Text)
ikprKeyFingerprint =
    lens _ikprKeyFingerprint (\s a -> s { _ikprKeyFingerprint = a })

-- | The key pair name you provided.
ikprKeyName :: Lens' ImportKeyPairResult (Maybe Text)
ikprKeyName = lens _ikprKeyName (\s a -> s { _ikprKeyName = a })

instance FromXML ImportKeyPairResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ImportKeyPairResult"

instance AWSRequest ImportKeyPair where
    type Sv ImportKeyPair = EC2
    type Rs ImportKeyPair = ImportKeyPairResult

    request  = post "ImportKeyPair"
    response = xmlResponse $ \h x -> ImportKeyPairResult
        <$> x %| "keyFingerprint"
        <*> x %| "keyName"
