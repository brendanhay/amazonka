{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteLogGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log group and permanently deletes all the archived
-- log events associated with the log group.
module Network.AWS.CloudWatchLogs.DeleteLogGroup
  ( -- * Creating a Request
    DeleteLogGroup (..),
    newDeleteLogGroup,

    -- * Request Lenses
    deleteLogGroup_logGroupName,

    -- * Destructuring the Response
    DeleteLogGroupResponse (..),
    newDeleteLogGroupResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLogGroup' smart constructor.
data DeleteLogGroup = DeleteLogGroup'
  { -- | The name of the log group.
    logGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'deleteLogGroup_logGroupName' - The name of the log group.
newDeleteLogGroup ::
  -- | 'logGroupName'
  Core.Text ->
  DeleteLogGroup
newDeleteLogGroup pLogGroupName_ =
  DeleteLogGroup' {logGroupName = pLogGroupName_}

-- | The name of the log group.
deleteLogGroup_logGroupName :: Lens.Lens' DeleteLogGroup Core.Text
deleteLogGroup_logGroupName = Lens.lens (\DeleteLogGroup' {logGroupName} -> logGroupName) (\s@DeleteLogGroup' {} a -> s {logGroupName = a} :: DeleteLogGroup)

instance Core.AWSRequest DeleteLogGroup where
  type
    AWSResponse DeleteLogGroup =
      DeleteLogGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteLogGroupResponse'

instance Core.Hashable DeleteLogGroup

instance Core.NFData DeleteLogGroup

instance Core.ToHeaders DeleteLogGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.DeleteLogGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteLogGroup where
  toJSON DeleteLogGroup' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("logGroupName" Core..= logGroupName)]
      )

instance Core.ToPath DeleteLogGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLogGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLogGroupResponse' smart constructor.
data DeleteLogGroupResponse = DeleteLogGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLogGroupResponse ::
  DeleteLogGroupResponse
newDeleteLogGroupResponse = DeleteLogGroupResponse'

instance Core.NFData DeleteLogGroupResponse
