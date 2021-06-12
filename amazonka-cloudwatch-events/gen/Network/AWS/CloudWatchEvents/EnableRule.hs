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
-- Module      : Network.AWS.CloudWatchEvents.EnableRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified rule. If the rule does not exist, the operation
-- fails.
--
-- When you enable a rule, incoming events might not immediately start
-- matching to a newly enabled rule. Allow a short period of time for
-- changes to take effect.
module Network.AWS.CloudWatchEvents.EnableRule
  ( -- * Creating a Request
    EnableRule (..),
    newEnableRule,

    -- * Request Lenses
    enableRule_eventBusName,
    enableRule_name,

    -- * Destructuring the Response
    EnableRuleResponse (..),
    newEnableRuleResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableRule' smart constructor.
data EnableRule = EnableRule'
  { -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Core.Maybe Core.Text,
    -- | The name of the rule.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusName', 'enableRule_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'name', 'enableRule_name' - The name of the rule.
newEnableRule ::
  -- | 'name'
  Core.Text ->
  EnableRule
newEnableRule pName_ =
  EnableRule'
    { eventBusName = Core.Nothing,
      name = pName_
    }

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
enableRule_eventBusName :: Lens.Lens' EnableRule (Core.Maybe Core.Text)
enableRule_eventBusName = Lens.lens (\EnableRule' {eventBusName} -> eventBusName) (\s@EnableRule' {} a -> s {eventBusName = a} :: EnableRule)

-- | The name of the rule.
enableRule_name :: Lens.Lens' EnableRule Core.Text
enableRule_name = Lens.lens (\EnableRule' {name} -> name) (\s@EnableRule' {} a -> s {name = a} :: EnableRule)

instance Core.AWSRequest EnableRule where
  type AWSResponse EnableRule = EnableRuleResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull EnableRuleResponse'

instance Core.Hashable EnableRule

instance Core.NFData EnableRule

instance Core.ToHeaders EnableRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.EnableRule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableRule where
  toJSON EnableRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventBusName" Core..=) Core.<$> eventBusName,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath EnableRule where
  toPath = Core.const "/"

instance Core.ToQuery EnableRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableRuleResponse' smart constructor.
data EnableRuleResponse = EnableRuleResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableRuleResponse ::
  EnableRuleResponse
newEnableRuleResponse = EnableRuleResponse'

instance Core.NFData EnableRuleResponse
