{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteServerCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified server certificate.
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

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteServerCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscServerCertificateName'
newtype DeleteServerCertificate = DeleteServerCertificate'
    { _dscServerCertificateName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteServerCertificate' smart constructor.
deleteServerCertificate :: Text -> DeleteServerCertificate
deleteServerCertificate pServerCertificateName_ =
    DeleteServerCertificate'
    { _dscServerCertificateName = pServerCertificateName_
    }

-- | The name of the server certificate you want to delete.
dscServerCertificateName :: Lens' DeleteServerCertificate Text
dscServerCertificateName = lens _dscServerCertificateName (\ s a -> s{_dscServerCertificateName = a});

instance AWSRequest DeleteServerCertificate where
        type Sv DeleteServerCertificate = IAM
        type Rs DeleteServerCertificate =
             DeleteServerCertificateResponse
        request = postQuery
        response
          = receiveNull DeleteServerCertificateResponse'

instance ToHeaders DeleteServerCertificate where
        toHeaders = const mempty

instance ToPath DeleteServerCertificate where
        toPath = const mempty

instance ToQuery DeleteServerCertificate where
        toQuery DeleteServerCertificate'{..}
          = mconcat
              ["Action" =:
                 ("DeleteServerCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "ServerCertificateName" =: _dscServerCertificateName]

-- | /See:/ 'deleteServerCertificateResponse' smart constructor.
data DeleteServerCertificateResponse =
    DeleteServerCertificateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteServerCertificateResponse' smart constructor.
deleteServerCertificateResponse :: DeleteServerCertificateResponse
deleteServerCertificateResponse = DeleteServerCertificateResponse'
