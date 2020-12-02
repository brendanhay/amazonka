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
-- Module      : Network.AWS.Glue.GetJobBookmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information on a job bookmark entry.
module Network.AWS.Glue.GetJobBookmark
  ( -- * Creating a Request
    getJobBookmark,
    GetJobBookmark,

    -- * Request Lenses
    gjbRunId,
    gjbJobName,

    -- * Destructuring the Response
    getJobBookmarkResponse,
    GetJobBookmarkResponse,

    -- * Response Lenses
    gjbrsJobBookmarkEntry,
    gjbrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJobBookmark' smart constructor.
data GetJobBookmark = GetJobBookmark'
  { _gjbRunId :: !(Maybe Text),
    _gjbJobName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetJobBookmark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjbRunId' - The unique run identifier associated with this job run.
--
-- * 'gjbJobName' - The name of the job in question.
getJobBookmark ::
  -- | 'gjbJobName'
  Text ->
  GetJobBookmark
getJobBookmark pJobName_ =
  GetJobBookmark' {_gjbRunId = Nothing, _gjbJobName = pJobName_}

-- | The unique run identifier associated with this job run.
gjbRunId :: Lens' GetJobBookmark (Maybe Text)
gjbRunId = lens _gjbRunId (\s a -> s {_gjbRunId = a})

-- | The name of the job in question.
gjbJobName :: Lens' GetJobBookmark Text
gjbJobName = lens _gjbJobName (\s a -> s {_gjbJobName = a})

instance AWSRequest GetJobBookmark where
  type Rs GetJobBookmark = GetJobBookmarkResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetJobBookmarkResponse'
            <$> (x .?> "JobBookmarkEntry") <*> (pure (fromEnum s))
      )

instance Hashable GetJobBookmark

instance NFData GetJobBookmark

instance ToHeaders GetJobBookmark where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetJobBookmark" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetJobBookmark where
  toJSON GetJobBookmark' {..} =
    object
      ( catMaybes
          [("RunId" .=) <$> _gjbRunId, Just ("JobName" .= _gjbJobName)]
      )

instance ToPath GetJobBookmark where
  toPath = const "/"

instance ToQuery GetJobBookmark where
  toQuery = const mempty

-- | /See:/ 'getJobBookmarkResponse' smart constructor.
data GetJobBookmarkResponse = GetJobBookmarkResponse'
  { _gjbrsJobBookmarkEntry ::
      !(Maybe JobBookmarkEntry),
    _gjbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetJobBookmarkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjbrsJobBookmarkEntry' - A structure that defines a point that a job can resume processing.
--
-- * 'gjbrsResponseStatus' - -- | The response status code.
getJobBookmarkResponse ::
  -- | 'gjbrsResponseStatus'
  Int ->
  GetJobBookmarkResponse
getJobBookmarkResponse pResponseStatus_ =
  GetJobBookmarkResponse'
    { _gjbrsJobBookmarkEntry = Nothing,
      _gjbrsResponseStatus = pResponseStatus_
    }

-- | A structure that defines a point that a job can resume processing.
gjbrsJobBookmarkEntry :: Lens' GetJobBookmarkResponse (Maybe JobBookmarkEntry)
gjbrsJobBookmarkEntry = lens _gjbrsJobBookmarkEntry (\s a -> s {_gjbrsJobBookmarkEntry = a})

-- | -- | The response status code.
gjbrsResponseStatus :: Lens' GetJobBookmarkResponse Int
gjbrsResponseStatus = lens _gjbrsResponseStatus (\s a -> s {_gjbrsResponseStatus = a})

instance NFData GetJobBookmarkResponse
