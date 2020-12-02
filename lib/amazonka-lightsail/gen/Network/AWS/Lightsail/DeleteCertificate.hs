{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SSL/TLS certificate for your Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- Certificates that are currently attached to a distribution cannot be deleted. Use the @DetachCertificateFromDistribution@ action to detach a certificate from a distribution.
module Network.AWS.Lightsail.DeleteCertificate
  ( -- * Creating a Request
    deleteCertificate,
    DeleteCertificate,

    -- * Request Lenses
    dcCertificateName,

    -- * Destructuring the Response
    deleteCertificateResponse,
    DeleteCertificateResponse,

    -- * Response Lenses
    dcrsOperations,
    dcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { _dcCertificateName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCertificateName' - The name of the certificate to delete. Use the @GetCertificates@ action to get a list of certificate names that you can specify.
deleteCertificate ::
  -- | 'dcCertificateName'
  Text ->
  DeleteCertificate
deleteCertificate pCertificateName_ =
  DeleteCertificate' {_dcCertificateName = pCertificateName_}

-- | The name of the certificate to delete. Use the @GetCertificates@ action to get a list of certificate names that you can specify.
dcCertificateName :: Lens' DeleteCertificate Text
dcCertificateName = lens _dcCertificateName (\s a -> s {_dcCertificateName = a})

instance AWSRequest DeleteCertificate where
  type Rs DeleteCertificate = DeleteCertificateResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DeleteCertificateResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteCertificate

instance NFData DeleteCertificate

instance ToHeaders DeleteCertificate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DeleteCertificate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteCertificate where
  toJSON DeleteCertificate' {..} =
    object
      (catMaybes [Just ("certificateName" .= _dcCertificateName)])

instance ToPath DeleteCertificate where
  toPath = const "/"

instance ToQuery DeleteCertificate where
  toQuery = const mempty

-- | /See:/ 'deleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  { _dcrsOperations ::
      !(Maybe [Operation]),
    _dcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
deleteCertificateResponse ::
  -- | 'dcrsResponseStatus'
  Int ->
  DeleteCertificateResponse
deleteCertificateResponse pResponseStatus_ =
  DeleteCertificateResponse'
    { _dcrsOperations = Nothing,
      _dcrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
dcrsOperations :: Lens' DeleteCertificateResponse [Operation]
dcrsOperations = lens _dcrsOperations (\s a -> s {_dcrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DeleteCertificateResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\s a -> s {_dcrsResponseStatus = a})

instance NFData DeleteCertificateResponse
