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
-- Module      : Network.AWS.CodeBuild.DeleteReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report group. Before you delete a report group, you must delete its reports.
module Network.AWS.CodeBuild.DeleteReportGroup
  ( -- * Creating a Request
    deleteReportGroup,
    DeleteReportGroup,

    -- * Request Lenses
    drgDeleteReports,
    drgArn,

    -- * Destructuring the Response
    deleteReportGroupResponse,
    DeleteReportGroupResponse,

    -- * Response Lenses
    drgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteReportGroup' smart constructor.
data DeleteReportGroup = DeleteReportGroup'
  { _drgDeleteReports ::
      !(Maybe Bool),
    _drgArn :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReportGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgDeleteReports' - If @true@ , deletes any reports that belong to a report group before deleting the report group.  If @false@ , you must delete any reports in the report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup> to get the reports in a report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport> to delete the reports. If you call @DeleteReportGroup@ for a report group that contains one or more reports, an exception is thrown.
--
-- * 'drgArn' - The ARN of the report group to delete.
deleteReportGroup ::
  -- | 'drgArn'
  Text ->
  DeleteReportGroup
deleteReportGroup pArn_ =
  DeleteReportGroup' {_drgDeleteReports = Nothing, _drgArn = pArn_}

-- | If @true@ , deletes any reports that belong to a report group before deleting the report group.  If @false@ , you must delete any reports in the report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup> to get the reports in a report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport> to delete the reports. If you call @DeleteReportGroup@ for a report group that contains one or more reports, an exception is thrown.
drgDeleteReports :: Lens' DeleteReportGroup (Maybe Bool)
drgDeleteReports = lens _drgDeleteReports (\s a -> s {_drgDeleteReports = a})

-- | The ARN of the report group to delete.
drgArn :: Lens' DeleteReportGroup Text
drgArn = lens _drgArn (\s a -> s {_drgArn = a})

instance AWSRequest DeleteReportGroup where
  type Rs DeleteReportGroup = DeleteReportGroupResponse
  request = postJSON codeBuild
  response =
    receiveEmpty
      (\s h x -> DeleteReportGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteReportGroup

instance NFData DeleteReportGroup

instance ToHeaders DeleteReportGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.DeleteReportGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteReportGroup where
  toJSON DeleteReportGroup' {..} =
    object
      ( catMaybes
          [ ("deleteReports" .=) <$> _drgDeleteReports,
            Just ("arn" .= _drgArn)
          ]
      )

instance ToPath DeleteReportGroup where
  toPath = const "/"

instance ToQuery DeleteReportGroup where
  toQuery = const mempty

-- | /See:/ 'deleteReportGroupResponse' smart constructor.
newtype DeleteReportGroupResponse = DeleteReportGroupResponse'
  { _drgrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReportGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgrsResponseStatus' - -- | The response status code.
deleteReportGroupResponse ::
  -- | 'drgrsResponseStatus'
  Int ->
  DeleteReportGroupResponse
deleteReportGroupResponse pResponseStatus_ =
  DeleteReportGroupResponse'
    { _drgrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drgrsResponseStatus :: Lens' DeleteReportGroupResponse Int
drgrsResponseStatus = lens _drgrsResponseStatus (\s a -> s {_drgrsResponseStatus = a})

instance NFData DeleteReportGroupResponse
