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
-- Module      : Network.AWS.CodeBuild.DeleteReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report.
module Network.AWS.CodeBuild.DeleteReport
  ( -- * Creating a Request
    deleteReport,
    DeleteReport,

    -- * Request Lenses
    drArn,

    -- * Destructuring the Response
    deleteReportResponse,
    DeleteReportResponse,

    -- * Response Lenses
    drrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteReport' smart constructor.
newtype DeleteReport = DeleteReport' {_drArn :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drArn' - The ARN of the report to delete.
deleteReport ::
  -- | 'drArn'
  Text ->
  DeleteReport
deleteReport pArn_ = DeleteReport' {_drArn = pArn_}

-- | The ARN of the report to delete.
drArn :: Lens' DeleteReport Text
drArn = lens _drArn (\s a -> s {_drArn = a})

instance AWSRequest DeleteReport where
  type Rs DeleteReport = DeleteReportResponse
  request = postJSON codeBuild
  response =
    receiveEmpty
      (\s h x -> DeleteReportResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteReport

instance NFData DeleteReport

instance ToHeaders DeleteReport where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.DeleteReport" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteReport where
  toJSON DeleteReport' {..} =
    object (catMaybes [Just ("arn" .= _drArn)])

instance ToPath DeleteReport where
  toPath = const "/"

instance ToQuery DeleteReport where
  toQuery = const mempty

-- | /See:/ 'deleteReportResponse' smart constructor.
newtype DeleteReportResponse = DeleteReportResponse'
  { _drrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsResponseStatus' - -- | The response status code.
deleteReportResponse ::
  -- | 'drrsResponseStatus'
  Int ->
  DeleteReportResponse
deleteReportResponse pResponseStatus_ =
  DeleteReportResponse' {_drrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
drrsResponseStatus :: Lens' DeleteReportResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\s a -> s {_drrsResponseStatus = a})

instance NFData DeleteReportResponse
