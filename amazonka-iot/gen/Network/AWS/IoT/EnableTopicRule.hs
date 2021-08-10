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
-- Module      : Network.AWS.IoT.EnableTopicRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the rule.
module Network.AWS.IoT.EnableTopicRule
  ( -- * Creating a Request
    EnableTopicRule (..),
    newEnableTopicRule,

    -- * Request Lenses
    enableTopicRule_ruleName,

    -- * Destructuring the Response
    EnableTopicRuleResponse (..),
    newEnableTopicRuleResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the EnableTopicRuleRequest operation.
--
-- /See:/ 'newEnableTopicRule' smart constructor.
data EnableTopicRule = EnableTopicRule'
  { -- | The name of the topic rule to enable.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableTopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'enableTopicRule_ruleName' - The name of the topic rule to enable.
newEnableTopicRule ::
  -- | 'ruleName'
  Prelude.Text ->
  EnableTopicRule
newEnableTopicRule pRuleName_ =
  EnableTopicRule' {ruleName = pRuleName_}

-- | The name of the topic rule to enable.
enableTopicRule_ruleName :: Lens.Lens' EnableTopicRule Prelude.Text
enableTopicRule_ruleName = Lens.lens (\EnableTopicRule' {ruleName} -> ruleName) (\s@EnableTopicRule' {} a -> s {ruleName = a} :: EnableTopicRule)

instance Core.AWSRequest EnableTopicRule where
  type
    AWSResponse EnableTopicRule =
      EnableTopicRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull EnableTopicRuleResponse'

instance Prelude.Hashable EnableTopicRule

instance Prelude.NFData EnableTopicRule

instance Core.ToHeaders EnableTopicRule where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON EnableTopicRule where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath EnableTopicRule where
  toPath EnableTopicRule' {..} =
    Prelude.mconcat
      ["/rules/", Core.toBS ruleName, "/enable"]

instance Core.ToQuery EnableTopicRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableTopicRuleResponse' smart constructor.
data EnableTopicRuleResponse = EnableTopicRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableTopicRuleResponse ::
  EnableTopicRuleResponse
newEnableTopicRuleResponse = EnableTopicRuleResponse'

instance Prelude.NFData EnableTopicRuleResponse
