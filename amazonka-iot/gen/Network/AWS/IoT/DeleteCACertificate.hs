{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteCACertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a registered CA certificate.
--
--
module Network.AWS.IoT.DeleteCACertificate
    (
    -- * Creating a Request
      deleteCACertificate
    , DeleteCACertificate
    -- * Request Lenses
    , dcacCertificateId

    -- * Destructuring the Response
    , deleteCACertificateResponse
    , DeleteCACertificateResponse
    -- * Response Lenses
    , dcacrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input for the DeleteCACertificate operation.
--
--
--
-- /See:/ 'deleteCACertificate' smart constructor.
newtype DeleteCACertificate = DeleteCACertificate'
  { _dcacCertificateId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCACertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcacCertificateId' - The ID of the certificate to delete. (The last part of the certificate ARN contains the certificate ID.)
deleteCACertificate
    :: Text -- ^ 'dcacCertificateId'
    -> DeleteCACertificate
deleteCACertificate pCertificateId_ =
  DeleteCACertificate' {_dcacCertificateId = pCertificateId_}


-- | The ID of the certificate to delete. (The last part of the certificate ARN contains the certificate ID.)
dcacCertificateId :: Lens' DeleteCACertificate Text
dcacCertificateId = lens _dcacCertificateId (\ s a -> s{_dcacCertificateId = a})

instance AWSRequest DeleteCACertificate where
        type Rs DeleteCACertificate =
             DeleteCACertificateResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteCACertificateResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteCACertificate where

instance NFData DeleteCACertificate where

instance ToHeaders DeleteCACertificate where
        toHeaders = const mempty

instance ToPath DeleteCACertificate where
        toPath DeleteCACertificate'{..}
          = mconcat
              ["/cacertificate/", toBS _dcacCertificateId]

instance ToQuery DeleteCACertificate where
        toQuery = const mempty

-- | The output for the DeleteCACertificate operation.
--
--
--
-- /See:/ 'deleteCACertificateResponse' smart constructor.
newtype DeleteCACertificateResponse = DeleteCACertificateResponse'
  { _dcacrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCACertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcacrsResponseStatus' - -- | The response status code.
deleteCACertificateResponse
    :: Int -- ^ 'dcacrsResponseStatus'
    -> DeleteCACertificateResponse
deleteCACertificateResponse pResponseStatus_ =
  DeleteCACertificateResponse' {_dcacrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcacrsResponseStatus :: Lens' DeleteCACertificateResponse Int
dcacrsResponseStatus = lens _dcacrsResponseStatus (\ s a -> s{_dcacrsResponseStatus = a})

instance NFData DeleteCACertificateResponse where
