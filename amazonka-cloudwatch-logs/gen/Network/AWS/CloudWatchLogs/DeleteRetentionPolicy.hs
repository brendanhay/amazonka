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
-- Module      : Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy.
--
-- Log events do not expire if they belong to log groups without a
-- retention policy.
module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
  ( -- * Creating a Request
    DeleteRetentionPolicy (..),
    newDeleteRetentionPolicy,

    -- * Request Lenses
    deleteRetentionPolicy_logGroupName,

    -- * Destructuring the Response
    DeleteRetentionPolicyResponse (..),
    newDeleteRetentionPolicyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRetentionPolicy' smart constructor.
data DeleteRetentionPolicy = DeleteRetentionPolicy'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRetentionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'deleteRetentionPolicy_logGroupName' - The name of the log group.
newDeleteRetentionPolicy ::
  -- | 'logGroupName'
  Prelude.Text ->
  DeleteRetentionPolicy
newDeleteRetentionPolicy pLogGroupName_ =
  DeleteRetentionPolicy'
    { logGroupName =
        pLogGroupName_
    }

-- | The name of the log group.
deleteRetentionPolicy_logGroupName :: Lens.Lens' DeleteRetentionPolicy Prelude.Text
deleteRetentionPolicy_logGroupName = Lens.lens (\DeleteRetentionPolicy' {logGroupName} -> logGroupName) (\s@DeleteRetentionPolicy' {} a -> s {logGroupName = a} :: DeleteRetentionPolicy)

instance Core.AWSRequest DeleteRetentionPolicy where
  type
    AWSResponse DeleteRetentionPolicy =
      DeleteRetentionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteRetentionPolicyResponse'

instance Prelude.Hashable DeleteRetentionPolicy

instance Prelude.NFData DeleteRetentionPolicy

instance Core.ToHeaders DeleteRetentionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DeleteRetentionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteRetentionPolicy where
  toJSON DeleteRetentionPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("logGroupName" Core..= logGroupName)]
      )

instance Core.ToPath DeleteRetentionPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteRetentionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRetentionPolicyResponse' smart constructor.
data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRetentionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRetentionPolicyResponse ::
  DeleteRetentionPolicyResponse
newDeleteRetentionPolicyResponse =
  DeleteRetentionPolicyResponse'

instance Prelude.NFData DeleteRetentionPolicyResponse
