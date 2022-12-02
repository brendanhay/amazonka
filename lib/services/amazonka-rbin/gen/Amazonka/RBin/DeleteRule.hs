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
-- Module      : Amazonka.RBin.DeleteRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Recycle Bin retention rule. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/recycle-bin-working-with-rules.html#recycle-bin-delete-rule Delete Recycle Bin retention rules>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.RBin.DeleteRule
  ( -- * Creating a Request
    DeleteRule (..),
    newDeleteRule,

    -- * Request Lenses
    deleteRule_identifier,

    -- * Destructuring the Response
    DeleteRuleResponse (..),
    newDeleteRuleResponse,

    -- * Response Lenses
    deleteRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The unique ID of the retention rule.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'deleteRule_identifier' - The unique ID of the retention rule.
newDeleteRule ::
  -- | 'identifier'
  Prelude.Text ->
  DeleteRule
newDeleteRule pIdentifier_ =
  DeleteRule' {identifier = pIdentifier_}

-- | The unique ID of the retention rule.
deleteRule_identifier :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_identifier = Lens.lens (\DeleteRule' {identifier} -> identifier) (\s@DeleteRule' {} a -> s {identifier = a} :: DeleteRule)

instance Core.AWSRequest DeleteRule where
  type AWSResponse DeleteRule = DeleteRuleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRule where
  hashWithSalt _salt DeleteRule' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeleteRule where
  rnf DeleteRule' {..} = Prelude.rnf identifier

instance Data.ToHeaders DeleteRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRule where
  toPath DeleteRule' {..} =
    Prelude.mconcat ["/rules/", Data.toBS identifier]

instance Data.ToQuery DeleteRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRuleResponse_httpStatus' - The response's http status code.
newDeleteRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRuleResponse
newDeleteRuleResponse pHttpStatus_ =
  DeleteRuleResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRuleResponse_httpStatus :: Lens.Lens' DeleteRuleResponse Prelude.Int
deleteRuleResponse_httpStatus = Lens.lens (\DeleteRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteRuleResponse' {} a -> s {httpStatus = a} :: DeleteRuleResponse)

instance Prelude.NFData DeleteRuleResponse where
  rnf DeleteRuleResponse' {..} = Prelude.rnf httpStatus
