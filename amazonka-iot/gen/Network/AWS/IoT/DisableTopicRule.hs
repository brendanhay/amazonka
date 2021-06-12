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
-- Module      : Network.AWS.IoT.DisableTopicRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the rule.
module Network.AWS.IoT.DisableTopicRule
  ( -- * Creating a Request
    DisableTopicRule (..),
    newDisableTopicRule,

    -- * Request Lenses
    disableTopicRule_ruleName,

    -- * Destructuring the Response
    DisableTopicRuleResponse (..),
    newDisableTopicRuleResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DisableTopicRuleRequest operation.
--
-- /See:/ 'newDisableTopicRule' smart constructor.
data DisableTopicRule = DisableTopicRule'
  { -- | The name of the rule to disable.
    ruleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableTopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'disableTopicRule_ruleName' - The name of the rule to disable.
newDisableTopicRule ::
  -- | 'ruleName'
  Core.Text ->
  DisableTopicRule
newDisableTopicRule pRuleName_ =
  DisableTopicRule' {ruleName = pRuleName_}

-- | The name of the rule to disable.
disableTopicRule_ruleName :: Lens.Lens' DisableTopicRule Core.Text
disableTopicRule_ruleName = Lens.lens (\DisableTopicRule' {ruleName} -> ruleName) (\s@DisableTopicRule' {} a -> s {ruleName = a} :: DisableTopicRule)

instance Core.AWSRequest DisableTopicRule where
  type
    AWSResponse DisableTopicRule =
      DisableTopicRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DisableTopicRuleResponse'

instance Core.Hashable DisableTopicRule

instance Core.NFData DisableTopicRule

instance Core.ToHeaders DisableTopicRule where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON DisableTopicRule where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DisableTopicRule where
  toPath DisableTopicRule' {..} =
    Core.mconcat
      ["/rules/", Core.toBS ruleName, "/disable"]

instance Core.ToQuery DisableTopicRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisableTopicRuleResponse' smart constructor.
data DisableTopicRuleResponse = DisableTopicRuleResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableTopicRuleResponse ::
  DisableTopicRuleResponse
newDisableTopicRuleResponse =
  DisableTopicRuleResponse'

instance Core.NFData DisableTopicRuleResponse
