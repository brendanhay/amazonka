{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteServerCertificate
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
module Network.AWS.IAM.DeleteServerCertificate
    (
    -- * Request
      DeleteServerCertificate
    -- ** Request constructor
    , deleteServerCertificate
    -- ** Request lenses
    , dscServerCertificateName

    -- * Response
    , DeleteServerCertificateResponse
    -- ** Response constructor
    , deleteServerCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype DeleteServerCertificate = DeleteServerCertificate
    { _dscServerCertificateName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteServerCertificate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ServerCertificateName ::@ @Text@
--
deleteServerCertificate :: Text -- ^ 'dscServerCertificateName'
                        -> DeleteServerCertificate
deleteServerCertificate p1 = DeleteServerCertificate
    { _dscServerCertificateName = p1
    }

-- | The name of the server certificate you want to delete.
dscServerCertificateName :: Lens' DeleteServerCertificate Text
dscServerCertificateName =
    lens _dscServerCertificateName
         (\s a -> s { _dscServerCertificateName = a })

instance ToQuery DeleteServerCertificate where
    toQuery = genericQuery def

data DeleteServerCertificateResponse = DeleteServerCertificateResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteServerCertificateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteServerCertificateResponse :: DeleteServerCertificateResponse
deleteServerCertificateResponse = DeleteServerCertificateResponse

instance AWSRequest DeleteServerCertificate where
    type Sv DeleteServerCertificate = IAM
    type Rs DeleteServerCertificate = DeleteServerCertificateResponse

    request = post "DeleteServerCertificate"
    response _ = nullaryResponse DeleteServerCertificateResponse
