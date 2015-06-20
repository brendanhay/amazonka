{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.DeleteServerCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified server certificate.
--
-- If you are using a server certificate with Elastic Load Balancing,
-- deleting the certificate could have implications for your application.
-- If Elastic Load Balancing doesn\'t detect the deletion of bound
-- certificates, it may continue to use the certificates. This could cause
-- Elastic Load Balancing to stop accepting traffic. We recommend that you
-- remove the reference to the certificate from Elastic Load Balancing
-- before using this command to delete the certificate. For more
-- information, go to
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html DeleteLoadBalancerListeners>
-- in the /Elastic Load Balancing API Reference/.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteServerCertificate.html>
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

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteServerCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscServerCertificateName'
newtype DeleteServerCertificate = DeleteServerCertificate'{_dscServerCertificateName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteServerCertificate' smart constructor.
deleteServerCertificate :: Text -> DeleteServerCertificate
deleteServerCertificate pServerCertificateName = DeleteServerCertificate'{_dscServerCertificateName = pServerCertificateName};

-- | The name of the server certificate you want to delete.
dscServerCertificateName :: Lens' DeleteServerCertificate Text
dscServerCertificateName = lens _dscServerCertificateName (\ s a -> s{_dscServerCertificateName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteServerCertificate where
        type Sv DeleteServerCertificate = IAM
        type Rs DeleteServerCertificate =
             DeleteServerCertificateResponse
        request = post
        response
          = receiveNull DeleteServerCertificateResponse'

instance ToHeaders DeleteServerCertificate where
        toHeaders = const mempty

instance ToPath DeleteServerCertificate where
        toPath = const "/"

instance ToQuery DeleteServerCertificate where
        toQuery DeleteServerCertificate'{..}
          = mconcat
              ["Action" =:
                 ("DeleteServerCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "ServerCertificateName" =: _dscServerCertificateName]

-- | /See:/ 'deleteServerCertificateResponse' smart constructor.
data DeleteServerCertificateResponse = DeleteServerCertificateResponse' deriving (Eq, Read, Show)

-- | 'DeleteServerCertificateResponse' smart constructor.
deleteServerCertificateResponse :: DeleteServerCertificateResponse
deleteServerCertificateResponse = DeleteServerCertificateResponse';
