{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CreateHsmClientCertificate
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
module Network.AWS.Redshift.V2012_12_01.CreateHsmClientCertificate where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data CreateHsmClientCertificate = CreateHsmClientCertificate
    { _chccmHsmClientCertificateIdentifier :: Text
      -- ^ The identifier to be assigned to the new HSM client certificate
      -- that the cluster will use to connect to the HSM to use the
      -- database encryption keys.
    } deriving (Show, Generic)

makeLenses ''CreateHsmClientCertificate

instance ToQuery CreateHsmClientCertificate where
    toQuery = genericQuery def

data CreateHsmClientCertificateResponse = CreateHsmClientCertificateResponse
    { _hccwHsmClientCertificate :: Maybe HsmClientCertificate
      -- ^ Returns information about an HSM client certificate. The
      -- certificate is stored in a secure Hardware Storage Module (HSM),
      -- and used by the Amazon Redshift cluster to encrypt data files.
    } deriving (Show, Generic)

makeLenses ''CreateHsmClientCertificateResponse

instance FromXML CreateHsmClientCertificateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateHsmClientCertificate where
    type Sv CreateHsmClientCertificate = Redshift
    type Rs CreateHsmClientCertificate = CreateHsmClientCertificateResponse

    request = post "CreateHsmClientCertificate"
    response _ = xmlResponse
