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
-- Module      : Amazonka.CloudWatchLogs.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy.
--
-- Log events do not expire if they belong to log groups without a
-- retention policy.
module Amazonka.CloudWatchLogs.DeleteRetentionPolicy
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteRetentionPolicyResponse'

instance Prelude.Hashable DeleteRetentionPolicy where
  hashWithSalt _salt DeleteRetentionPolicy' {..} =
    _salt `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData DeleteRetentionPolicy where
  rnf DeleteRetentionPolicy' {..} =
    Prelude.rnf logGroupName

instance Data.ToHeaders DeleteRetentionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DeleteRetentionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRetentionPolicy where
  toJSON DeleteRetentionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("logGroupName" Data..= logGroupName)]
      )

instance Data.ToPath DeleteRetentionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRetentionPolicy where
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

instance Prelude.NFData DeleteRetentionPolicyResponse where
  rnf _ = ()
