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
-- Module      : Network.AWS.CloudWatchEvents.DisableRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified rule. A disabled rule won\'t match any events,
-- and won\'t self-trigger if it has a schedule expression.
--
-- When you disable a rule, incoming events might continue to match to the
-- disabled rule. Allow a short period of time for changes to take effect.
module Network.AWS.CloudWatchEvents.DisableRule
  ( -- * Creating a Request
    DisableRule (..),
    newDisableRule,

    -- * Request Lenses
    disableRule_eventBusName,
    disableRule_name,

    -- * Destructuring the Response
    DisableRuleResponse (..),
    newDisableRuleResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableRule' smart constructor.
data DisableRule = DisableRule'
  { -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Core.Maybe Core.Text,
    -- | The name of the rule.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusName', 'disableRule_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'name', 'disableRule_name' - The name of the rule.
newDisableRule ::
  -- | 'name'
  Core.Text ->
  DisableRule
newDisableRule pName_ =
  DisableRule'
    { eventBusName = Core.Nothing,
      name = pName_
    }

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
disableRule_eventBusName :: Lens.Lens' DisableRule (Core.Maybe Core.Text)
disableRule_eventBusName = Lens.lens (\DisableRule' {eventBusName} -> eventBusName) (\s@DisableRule' {} a -> s {eventBusName = a} :: DisableRule)

-- | The name of the rule.
disableRule_name :: Lens.Lens' DisableRule Core.Text
disableRule_name = Lens.lens (\DisableRule' {name} -> name) (\s@DisableRule' {} a -> s {name = a} :: DisableRule)

instance Core.AWSRequest DisableRule where
  type AWSResponse DisableRule = DisableRuleResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DisableRuleResponse'

instance Core.Hashable DisableRule

instance Core.NFData DisableRule

instance Core.ToHeaders DisableRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DisableRule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableRule where
  toJSON DisableRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventBusName" Core..=) Core.<$> eventBusName,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath DisableRule where
  toPath = Core.const "/"

instance Core.ToQuery DisableRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisableRuleResponse' smart constructor.
data DisableRuleResponse = DisableRuleResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableRuleResponse ::
  DisableRuleResponse
newDisableRuleResponse = DisableRuleResponse'

instance Core.NFData DisableRuleResponse
