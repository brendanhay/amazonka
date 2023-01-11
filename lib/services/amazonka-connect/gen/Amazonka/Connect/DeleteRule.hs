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
-- Module      : Amazonka.Connect.DeleteRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a rule for the specified Amazon Connect instance.
module Amazonka.Connect.DeleteRule
  ( -- * Creating a Request
    DeleteRule (..),
    newDeleteRule,

    -- * Request Lenses
    deleteRule_instanceId,
    deleteRule_ruleId,

    -- * Destructuring the Response
    DeleteRuleResponse (..),
    newDeleteRuleResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the rule.
    ruleId :: Prelude.Text
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
-- 'instanceId', 'deleteRule_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'ruleId', 'deleteRule_ruleId' - A unique identifier for the rule.
newDeleteRule ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'ruleId'
  Prelude.Text ->
  DeleteRule
newDeleteRule pInstanceId_ pRuleId_ =
  DeleteRule'
    { instanceId = pInstanceId_,
      ruleId = pRuleId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteRule_instanceId :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_instanceId = Lens.lens (\DeleteRule' {instanceId} -> instanceId) (\s@DeleteRule' {} a -> s {instanceId = a} :: DeleteRule)

-- | A unique identifier for the rule.
deleteRule_ruleId :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_ruleId = Lens.lens (\DeleteRule' {ruleId} -> ruleId) (\s@DeleteRule' {} a -> s {ruleId = a} :: DeleteRule)

instance Core.AWSRequest DeleteRule where
  type AWSResponse DeleteRule = DeleteRuleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteRuleResponse'

instance Prelude.Hashable DeleteRule where
  hashWithSalt _salt DeleteRule' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` ruleId

instance Prelude.NFData DeleteRule where
  rnf DeleteRule' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf ruleId

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
    Prelude.mconcat
      [ "/rules/",
        Data.toBS instanceId,
        "/",
        Data.toBS ruleId
      ]

instance Data.ToQuery DeleteRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRuleResponse ::
  DeleteRuleResponse
newDeleteRuleResponse = DeleteRuleResponse'

instance Prelude.NFData DeleteRuleResponse where
  rnf _ = ()
