{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteServerCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified server certificate. If you are using a server
-- certificate with Elastic Load Balancing, deleting the certificate could
-- have implications for your application. If Elastic Load Balancing doesn't
-- detect the deletion of bound certificates, it may continue to use the
-- certificates. This could cause Elastic Load Balancing to stop accepting
-- traffic. We recommend that you remove the reference to the certificate from
-- Elastic Load Balancing before using this command to delete the certificate.
-- For more information, go to DeleteLoadBalancerListeners in the Elastic Load
-- Balancing API Reference. https://iam.amazonaws.com/
-- ?Action=DeleteServerCertificate &ServerCertificateName=ProdServerCert
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteServerCertificate
    (
    -- * Request
      DeleteServerCertificate
    -- ** Request constructor
    , deleteServerCertificate
    -- ** Request lenses
    , dscrServerCertificateName

    -- * Response
    , DeleteServerCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteServerCertificate' request.
deleteServerCertificate :: Text -- ^ 'dscrServerCertificateName'
                        -> DeleteServerCertificate
deleteServerCertificate p1 = DeleteServerCertificate
    { _dscrServerCertificateName = p1
    }
{-# INLINE deleteServerCertificate #-}

data DeleteServerCertificate = DeleteServerCertificate
    { _dscrServerCertificateName :: Text
      -- ^ The name of the server certificate you want to delete.
    } deriving (Show, Generic)

-- | The name of the server certificate you want to delete.
dscrServerCertificateName :: Lens' DeleteServerCertificate (Text)
dscrServerCertificateName f x =
    f (_dscrServerCertificateName x)
        <&> \y -> x { _dscrServerCertificateName = y }
{-# INLINE dscrServerCertificateName #-}

instance ToQuery DeleteServerCertificate where
    toQuery = genericQuery def

data DeleteServerCertificateResponse = DeleteServerCertificateResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteServerCertificate where
    type Sv DeleteServerCertificate = IAM
    type Rs DeleteServerCertificate = DeleteServerCertificateResponse

    request = post "DeleteServerCertificate"
    response _ = nullaryResponse DeleteServerCertificateResponse
