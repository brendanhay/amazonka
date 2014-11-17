{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Creates an HSM client certificate that an Amazon Redshift cluster will use
-- to connect to the client's HSM in order to store and retrieve the keys used
-- to encrypt the cluster databases. The command returns a public key, which
-- you must store in the HSM. In addition to creating the HSM certificate, you
-- must create an Amazon Redshift HSM configuration that provides a cluster
-- the information needed to store and use encryption keys in the HSM. For
-- more information, go to Hardware Security Modules in the Amazon Redshift
-- Management Guide.
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

newtype CreateHsmClientCertificate = CreateHsmClientCertificate
    { _chccHsmClientCertificateIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'CreateHsmClientCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chccHsmClientCertificateIdentifier' @::@ 'Text'
--
createHsmClientCertificate :: Text -- ^ 'chccHsmClientCertificateIdentifier'
                           -> CreateHsmClientCertificate
createHsmClientCertificate p1 = CreateHsmClientCertificate
    { _chccHsmClientCertificateIdentifier = p1
    }

-- | The identifier to be assigned to the new HSM client certificate that the
-- cluster will use to connect to the HSM to use the database encryption
-- keys.
chccHsmClientCertificateIdentifier :: Lens' CreateHsmClientCertificate Text
chccHsmClientCertificateIdentifier =
    lens _chccHsmClientCertificateIdentifier
        (\s a -> s { _chccHsmClientCertificateIdentifier = a })

newtype CreateHsmClientCertificateResponse = CreateHsmClientCertificateResponse
    { _chccrHsmClientCertificate :: Maybe HsmClientCertificate
    } deriving (Eq, Show, Generic)

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

instance ToQuery CreateHsmClientCertificate

instance ToHeaders CreateHsmClientCertificate

instance AWSRequest CreateHsmClientCertificate where
    type Sv CreateHsmClientCertificate = Redshift
    type Rs CreateHsmClientCertificate = CreateHsmClientCertificateResponse

    request  = post "CreateHsmClientCertificate"
    response = xmlResponse

instance FromXML CreateHsmClientCertificateResponse where
    parseXML c = CreateHsmClientCertificateResponse
        <$> c .:? "HsmClientCertificate"
