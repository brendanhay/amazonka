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
-- Module      : Amazonka.IoT.DeleteTopicRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the rule.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteTopicRule>
-- action.
module Amazonka.IoT.DeleteTopicRule
  ( -- * Creating a Request
    DeleteTopicRule (..),
    newDeleteTopicRule,

    -- * Request Lenses
    deleteTopicRule_ruleName,

    -- * Destructuring the Response
    DeleteTopicRuleResponse (..),
    newDeleteTopicRuleResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeleteTopicRule operation.
--
-- /See:/ 'newDeleteTopicRule' smart constructor.
data DeleteTopicRule = DeleteTopicRule'
  { -- | The name of the rule.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'deleteTopicRule_ruleName' - The name of the rule.
newDeleteTopicRule ::
  -- | 'ruleName'
  Prelude.Text ->
  DeleteTopicRule
newDeleteTopicRule pRuleName_ =
  DeleteTopicRule' {ruleName = pRuleName_}

-- | The name of the rule.
deleteTopicRule_ruleName :: Lens.Lens' DeleteTopicRule Prelude.Text
deleteTopicRule_ruleName = Lens.lens (\DeleteTopicRule' {ruleName} -> ruleName) (\s@DeleteTopicRule' {} a -> s {ruleName = a} :: DeleteTopicRule)

instance Core.AWSRequest DeleteTopicRule where
  type
    AWSResponse DeleteTopicRule =
      DeleteTopicRuleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteTopicRuleResponse'

instance Prelude.Hashable DeleteTopicRule where
  hashWithSalt _salt DeleteTopicRule' {..} =
    _salt `Prelude.hashWithSalt` ruleName

instance Prelude.NFData DeleteTopicRule where
  rnf DeleteTopicRule' {..} = Prelude.rnf ruleName

instance Data.ToHeaders DeleteTopicRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTopicRule where
  toPath DeleteTopicRule' {..} =
    Prelude.mconcat ["/rules/", Data.toBS ruleName]

instance Data.ToQuery DeleteTopicRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTopicRuleResponse' smart constructor.
data DeleteTopicRuleResponse = DeleteTopicRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTopicRuleResponse ::
  DeleteTopicRuleResponse
newDeleteTopicRuleResponse = DeleteTopicRuleResponse'

instance Prelude.NFData DeleteTopicRuleResponse where
  rnf _ = ()
