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

-- Module      : Network.AWS.Redshift.CreateHsmClientCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an HSM client certificate that an Amazon Redshift cluster will use
-- to connect to the client's HSM in order to store and retrieve the keys used
-- to encrypt the cluster databases. The command returns a public key, which
-- you must store in the HSM. In addition to creating the HSM certificate, you
-- must create an Amazon Redshift HSM configuration that provides a cluster
-- the information needed to store and use encryption keys in the HSM. For
-- more information, go to Hardware Security Modules in the Amazon Redshift
-- Management Guide.
module Network.AWS.Redshift.CreateHsmClientCertificate
    (
    -- * Request
      CreateHsmClientCertificateMessage
    -- ** Request constructor
    , createHsmClientCertificate
    -- ** Request lenses
    , chccmHsmClientCertificateIdentifier

    -- * Response
    , CreateHsmClientCertificateResult
    -- ** Response constructor
    , createHsmClientCertificateResponse
    -- ** Response lenses
    , chccrHsmClientCertificate
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype CreateHsmClientCertificateMessage = CreateHsmClientCertificateMessage
    { _chccmHsmClientCertificateIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'CreateHsmClientCertificateMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccmHsmClientCertificateIdentifier' @::@ 'Text'
--
createHsmClientCertificate :: Text -- ^ 'chccmHsmClientCertificateIdentifier'
                           -> CreateHsmClientCertificateMessage
createHsmClientCertificate p1 = CreateHsmClientCertificateMessage
    { _chccmHsmClientCertificateIdentifier = p1
    }

-- | The identifier to be assigned to the new HSM client certificate that the
-- cluster will use to connect to the HSM to use the database encryption
-- keys.
chccmHsmClientCertificateIdentifier :: Lens' CreateHsmClientCertificateMessage Text
chccmHsmClientCertificateIdentifier =
    lens _chccmHsmClientCertificateIdentifier
        (\s a -> s { _chccmHsmClientCertificateIdentifier = a })

instance ToQuery CreateHsmClientCertificateMessage

instance ToPath CreateHsmClientCertificateMessage where
    toPath = const "/"

newtype CreateHsmClientCertificateResult = CreateHsmClientCertificateResult
    { _chccrHsmClientCertificate :: Maybe HsmClientCertificate
    } deriving (Eq, Show, Generic)

-- | 'CreateHsmClientCertificateResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccrHsmClientCertificate' @::@ 'Maybe' 'HsmClientCertificate'
--
createHsmClientCertificateResponse :: CreateHsmClientCertificateResult
createHsmClientCertificateResponse = CreateHsmClientCertificateResult
    { _chccrHsmClientCertificate = Nothing
    }

chccrHsmClientCertificate :: Lens' CreateHsmClientCertificateResult (Maybe HsmClientCertificate)
chccrHsmClientCertificate =
    lens _chccrHsmClientCertificate
        (\s a -> s { _chccrHsmClientCertificate = a })

instance FromXML CreateHsmClientCertificateResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateHsmClientCertificateResult"

instance AWSRequest CreateHsmClientCertificateMessage where
    type Sv CreateHsmClientCertificateMessage = Redshift
    type Rs CreateHsmClientCertificateMessage = CreateHsmClientCertificateResult

    request  = post "CreateHsmClientCertificate"
    response = xmlResponse $ \h x -> CreateHsmClientCertificateResult
        <$> x %| "HsmClientCertificate"
