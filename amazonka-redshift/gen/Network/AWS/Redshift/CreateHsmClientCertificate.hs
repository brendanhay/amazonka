{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateHsmClientCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an HSM client certificate that an Amazon Redshift cluster will use to
-- connect to the client's HSM in order to store and retrieve the keys used to
-- encrypt the cluster databases.
--
-- The command returns a public key, which you must store in the HSM. In
-- addition to creating the HSM certificate, you must create an Amazon Redshift
-- HSM configuration that provides a cluster the information needed to store and
-- use encryption keys in the HSM. For more information, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware SecurityModules> in the Amazon Redshift Cluster Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateHsmClientCertificate.html>
module Network.AWS.Redshift.CreateHsmClientCertificate
    (
    -- * Request
      CreateHsmClientCertificate
    -- ** Request constructor
    , createHsmClientCertificate
    -- ** Request lenses
    , chccHsmClientCertificateIdentifier
    , chccTags

    -- * Response
    , CreateHsmClientCertificateResponse
    -- ** Response constructor
    , createHsmClientCertificateResponse
    -- ** Response lenses
    , chccrHsmClientCertificate
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateHsmClientCertificate = CreateHsmClientCertificate
    { _chccHsmClientCertificateIdentifier :: Text
    , _chccTags                           :: List "Tag" Tag
    } deriving (Eq, Show)

-- | 'CreateHsmClientCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccHsmClientCertificateIdentifier' @::@ 'Text'
--
-- * 'chccTags' @::@ ['Tag']
--
createHsmClientCertificate :: Text -- ^ 'chccHsmClientCertificateIdentifier'
                           -> CreateHsmClientCertificate
createHsmClientCertificate p1 = CreateHsmClientCertificate
    { _chccHsmClientCertificateIdentifier = p1
    , _chccTags                           = mempty
    }

-- | The identifier to be assigned to the new HSM client certificate that the
-- cluster will use to connect to the HSM to use the database encryption keys.
chccHsmClientCertificateIdentifier :: Lens' CreateHsmClientCertificate Text
chccHsmClientCertificateIdentifier =
    lens _chccHsmClientCertificateIdentifier
        (\s a -> s { _chccHsmClientCertificateIdentifier = a })

-- | A list of tag instances.
chccTags :: Lens' CreateHsmClientCertificate [Tag]
chccTags = lens _chccTags (\s a -> s { _chccTags = a }) . _List

newtype CreateHsmClientCertificateResponse = CreateHsmClientCertificateResponse
    { _chccrHsmClientCertificate :: Maybe HsmClientCertificate
    } deriving (Eq, Show)

-- | 'CreateHsmClientCertificateResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccrHsmClientCertificate' @::@ 'Maybe' 'HsmClientCertificate'
--
createHsmClientCertificateResponse :: CreateHsmClientCertificateResponse
createHsmClientCertificateResponse = CreateHsmClientCertificateResponse
    { _chccrHsmClientCertificate = Nothing
    }

chccrHsmClientCertificate :: Lens' CreateHsmClientCertificateResponse (Maybe HsmClientCertificate)
chccrHsmClientCertificate =
    lens _chccrHsmClientCertificate
        (\s a -> s { _chccrHsmClientCertificate = a })

instance ToPath CreateHsmClientCertificate where
    toPath = const "/"

instance ToQuery CreateHsmClientCertificate where
    toQuery CreateHsmClientCertificate{..} = mconcat
        [ "HsmClientCertificateIdentifier" =? _chccHsmClientCertificateIdentifier
        , "Tags"                           =? _chccTags
        ]

instance ToHeaders CreateHsmClientCertificate

instance AWSRequest CreateHsmClientCertificate where
    type Sv CreateHsmClientCertificate = Redshift
    type Rs CreateHsmClientCertificate = CreateHsmClientCertificateResponse

    request  = post "CreateHsmClientCertificate"
    response = xmlResponse

instance FromXML CreateHsmClientCertificateResponse where
    parseXML = withElement "CreateHsmClientCertificateResult" $ \x -> CreateHsmClientCertificateResponse
        <$> x .@? "HsmClientCertificate"
