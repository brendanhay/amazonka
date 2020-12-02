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
-- Module      : Network.AWS.Pinpoint.CreateImportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import job for an application.
module Network.AWS.Pinpoint.CreateImportJob
  ( -- * Creating a Request
    createImportJob,
    CreateImportJob,

    -- * Request Lenses
    cijApplicationId,
    cijImportJobRequest,

    -- * Destructuring the Response
    createImportJobResponse,
    CreateImportJobResponse,

    -- * Response Lenses
    cijrsResponseStatus,
    cijrsImportJobResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createImportJob' smart constructor.
data CreateImportJob = CreateImportJob'
  { _cijApplicationId :: !Text,
    _cijImportJobRequest :: !ImportJobRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateImportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cijApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'cijImportJobRequest' - Undocumented member.
createImportJob ::
  -- | 'cijApplicationId'
  Text ->
  -- | 'cijImportJobRequest'
  ImportJobRequest ->
  CreateImportJob
createImportJob pApplicationId_ pImportJobRequest_ =
  CreateImportJob'
    { _cijApplicationId = pApplicationId_,
      _cijImportJobRequest = pImportJobRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
cijApplicationId :: Lens' CreateImportJob Text
cijApplicationId = lens _cijApplicationId (\s a -> s {_cijApplicationId = a})

-- | Undocumented member.
cijImportJobRequest :: Lens' CreateImportJob ImportJobRequest
cijImportJobRequest = lens _cijImportJobRequest (\s a -> s {_cijImportJobRequest = a})

instance AWSRequest CreateImportJob where
  type Rs CreateImportJob = CreateImportJobResponse
  request = postJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          CreateImportJobResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable CreateImportJob

instance NFData CreateImportJob

instance ToHeaders CreateImportJob where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateImportJob where
  toJSON CreateImportJob' {..} =
    object
      (catMaybes [Just ("ImportJobRequest" .= _cijImportJobRequest)])

instance ToPath CreateImportJob where
  toPath CreateImportJob' {..} =
    mconcat ["/v1/apps/", toBS _cijApplicationId, "/jobs/import"]

instance ToQuery CreateImportJob where
  toQuery = const mempty

-- | /See:/ 'createImportJobResponse' smart constructor.
data CreateImportJobResponse = CreateImportJobResponse'
  { _cijrsResponseStatus ::
      !Int,
    _cijrsImportJobResponse ::
      !ImportJobResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cijrsResponseStatus' - -- | The response status code.
--
-- * 'cijrsImportJobResponse' - Undocumented member.
createImportJobResponse ::
  -- | 'cijrsResponseStatus'
  Int ->
  -- | 'cijrsImportJobResponse'
  ImportJobResponse ->
  CreateImportJobResponse
createImportJobResponse pResponseStatus_ pImportJobResponse_ =
  CreateImportJobResponse'
    { _cijrsResponseStatus = pResponseStatus_,
      _cijrsImportJobResponse = pImportJobResponse_
    }

-- | -- | The response status code.
cijrsResponseStatus :: Lens' CreateImportJobResponse Int
cijrsResponseStatus = lens _cijrsResponseStatus (\s a -> s {_cijrsResponseStatus = a})

-- | Undocumented member.
cijrsImportJobResponse :: Lens' CreateImportJobResponse ImportJobResponse
cijrsImportJobResponse = lens _cijrsImportJobResponse (\s a -> s {_cijrsImportJobResponse = a})

instance NFData CreateImportJobResponse
