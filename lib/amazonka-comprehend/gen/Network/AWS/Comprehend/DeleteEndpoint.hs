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
-- Module      : Network.AWS.Comprehend.DeleteEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model-specific endpoint for a previously-trained custom model. All endpoints must be deleted in order for the model to be deleted.
module Network.AWS.Comprehend.DeleteEndpoint
  ( -- * Creating a Request
    deleteEndpoint,
    DeleteEndpoint,

    -- * Request Lenses
    deEndpointARN,

    -- * Destructuring the Response
    deleteEndpointResponse,
    DeleteEndpointResponse,

    -- * Response Lenses
    delrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint' {_deEndpointARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEndpointARN' - The Amazon Resource Number (ARN) of the endpoint being deleted.
deleteEndpoint ::
  -- | 'deEndpointARN'
  Text ->
  DeleteEndpoint
deleteEndpoint pEndpointARN_ =
  DeleteEndpoint' {_deEndpointARN = pEndpointARN_}

-- | The Amazon Resource Number (ARN) of the endpoint being deleted.
deEndpointARN :: Lens' DeleteEndpoint Text
deEndpointARN = lens _deEndpointARN (\s a -> s {_deEndpointARN = a})

instance AWSRequest DeleteEndpoint where
  type Rs DeleteEndpoint = DeleteEndpointResponse
  request = postJSON comprehend
  response =
    receiveEmpty
      (\s h x -> DeleteEndpointResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteEndpoint

instance NFData DeleteEndpoint

instance ToHeaders DeleteEndpoint where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DeleteEndpoint" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteEndpoint where
  toJSON DeleteEndpoint' {..} =
    object (catMaybes [Just ("EndpointArn" .= _deEndpointARN)])

instance ToPath DeleteEndpoint where
  toPath = const "/"

instance ToQuery DeleteEndpoint where
  toQuery = const mempty

-- | /See:/ 'deleteEndpointResponse' smart constructor.
newtype DeleteEndpointResponse = DeleteEndpointResponse'
  { _delrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteEndpointResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteEndpointResponse
deleteEndpointResponse pResponseStatus_ =
  DeleteEndpointResponse' {_delrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteEndpointResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteEndpointResponse
