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
-- Module      : Amazonka.CloudWatchLogs.DeleteDataProtectionPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the data protection policy from the specified log group.
--
-- For more information about data protection policies, see
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDataProtectionPolicy.html PutDataProtectionPolicy>.
module Amazonka.CloudWatchLogs.DeleteDataProtectionPolicy
  ( -- * Creating a Request
    DeleteDataProtectionPolicy (..),
    newDeleteDataProtectionPolicy,

    -- * Request Lenses
    deleteDataProtectionPolicy_logGroupIdentifier,

    -- * Destructuring the Response
    DeleteDataProtectionPolicyResponse (..),
    newDeleteDataProtectionPolicyResponse,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataProtectionPolicy' smart constructor.
data DeleteDataProtectionPolicy = DeleteDataProtectionPolicy'
  { -- | The name or ARN of the log group that you want to delete the data
    -- protection policy for.
    logGroupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataProtectionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupIdentifier', 'deleteDataProtectionPolicy_logGroupIdentifier' - The name or ARN of the log group that you want to delete the data
-- protection policy for.
newDeleteDataProtectionPolicy ::
  -- | 'logGroupIdentifier'
  Prelude.Text ->
  DeleteDataProtectionPolicy
newDeleteDataProtectionPolicy pLogGroupIdentifier_ =
  DeleteDataProtectionPolicy'
    { logGroupIdentifier =
        pLogGroupIdentifier_
    }

-- | The name or ARN of the log group that you want to delete the data
-- protection policy for.
deleteDataProtectionPolicy_logGroupIdentifier :: Lens.Lens' DeleteDataProtectionPolicy Prelude.Text
deleteDataProtectionPolicy_logGroupIdentifier = Lens.lens (\DeleteDataProtectionPolicy' {logGroupIdentifier} -> logGroupIdentifier) (\s@DeleteDataProtectionPolicy' {} a -> s {logGroupIdentifier = a} :: DeleteDataProtectionPolicy)

instance Core.AWSRequest DeleteDataProtectionPolicy where
  type
    AWSResponse DeleteDataProtectionPolicy =
      DeleteDataProtectionPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteDataProtectionPolicyResponse'

instance Prelude.Hashable DeleteDataProtectionPolicy where
  hashWithSalt _salt DeleteDataProtectionPolicy' {..} =
    _salt `Prelude.hashWithSalt` logGroupIdentifier

instance Prelude.NFData DeleteDataProtectionPolicy where
  rnf DeleteDataProtectionPolicy' {..} =
    Prelude.rnf logGroupIdentifier

instance Data.ToHeaders DeleteDataProtectionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DeleteDataProtectionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataProtectionPolicy where
  toJSON DeleteDataProtectionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("logGroupIdentifier" Data..= logGroupIdentifier)
          ]
      )

instance Data.ToPath DeleteDataProtectionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDataProtectionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataProtectionPolicyResponse' smart constructor.
data DeleteDataProtectionPolicyResponse = DeleteDataProtectionPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataProtectionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDataProtectionPolicyResponse ::
  DeleteDataProtectionPolicyResponse
newDeleteDataProtectionPolicyResponse =
  DeleteDataProtectionPolicyResponse'

instance
  Prelude.NFData
    DeleteDataProtectionPolicyResponse
  where
  rnf _ = ()
