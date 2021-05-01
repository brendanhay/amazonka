{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLogGroup' smart constructor.
data DeleteLogGroup = DeleteLogGroup'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteLogGroup
newDeleteLogGroup pLogGroupName_ =
  DeleteLogGroup' {logGroupName = pLogGroupName_}

-- | The name of the log group.
deleteLogGroup_logGroupName :: Lens.Lens' DeleteLogGroup Prelude.Text
deleteLogGroup_logGroupName = Lens.lens (\DeleteLogGroup' {logGroupName} -> logGroupName) (\s@DeleteLogGroup' {} a -> s {logGroupName = a} :: DeleteLogGroup)

instance Prelude.AWSRequest DeleteLogGroup where
  type Rs DeleteLogGroup = DeleteLogGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteLogGroupResponse'

instance Prelude.Hashable DeleteLogGroup

instance Prelude.NFData DeleteLogGroup

instance Prelude.ToHeaders DeleteLogGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.DeleteLogGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteLogGroup where
  toJSON DeleteLogGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("logGroupName" Prelude..= logGroupName)
          ]
      )

instance Prelude.ToPath DeleteLogGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLogGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLogGroupResponse' smart constructor.
data DeleteLogGroupResponse = DeleteLogGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLogGroupResponse ::
  DeleteLogGroupResponse
newDeleteLogGroupResponse = DeleteLogGroupResponse'

instance Prelude.NFData DeleteLogGroupResponse
