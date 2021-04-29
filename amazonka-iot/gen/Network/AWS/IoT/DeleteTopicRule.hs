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
-- Module      : Network.AWS.IoT.DeleteTopicRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the rule.
module Network.AWS.IoT.DeleteTopicRule
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteTopicRule operation.
--
-- /See:/ 'newDeleteTopicRule' smart constructor.
data DeleteTopicRule = DeleteTopicRule'
  { -- | The name of the rule.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteTopicRule where
  type Rs DeleteTopicRule = DeleteTopicRuleResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteTopicRuleResponse'

instance Prelude.Hashable DeleteTopicRule

instance Prelude.NFData DeleteTopicRule

instance Prelude.ToHeaders DeleteTopicRule where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTopicRule where
  toPath DeleteTopicRule' {..} =
    Prelude.mconcat ["/rules/", Prelude.toBS ruleName]

instance Prelude.ToQuery DeleteTopicRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTopicRuleResponse' smart constructor.
data DeleteTopicRuleResponse = DeleteTopicRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTopicRuleResponse ::
  DeleteTopicRuleResponse
newDeleteTopicRuleResponse = DeleteTopicRuleResponse'

instance Prelude.NFData DeleteTopicRuleResponse
