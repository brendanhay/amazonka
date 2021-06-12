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
-- Module      : Network.AWS.CloudWatchEvents.DeleteRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
-- Before you can delete the rule, you must remove all targets, using
-- RemoveTargets.
--
-- When you delete a rule, incoming events might continue to match to the
-- deleted rule. Allow a short period of time for changes to take effect.
--
-- Managed rules are rules created and managed by another AWS service on
-- your behalf. These rules are created by those other AWS services to
-- support functionality in those services. You can delete these rules
-- using the @Force@ option, but you should do so only if you are sure the
-- other service is not still using that rule.
module Network.AWS.CloudWatchEvents.DeleteRule
  ( -- * Creating a Request
    DeleteRule (..),
    newDeleteRule,

    -- * Request Lenses
    deleteRule_force,
    deleteRule_eventBusName,
    deleteRule_name,

    -- * Destructuring the Response
    DeleteRuleResponse (..),
    newDeleteRuleResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | If this is a managed rule, created by an AWS service on your behalf, you
    -- must specify @Force@ as @True@ to delete the rule. This parameter is
    -- ignored for rules that are not managed rules. You can check whether a
    -- rule is a managed rule by using @DescribeRule@ or @ListRules@ and
    -- checking the @ManagedBy@ field of the response.
    force :: Core.Maybe Core.Bool,
    -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Core.Maybe Core.Text,
    -- | The name of the rule.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deleteRule_force' - If this is a managed rule, created by an AWS service on your behalf, you
-- must specify @Force@ as @True@ to delete the rule. This parameter is
-- ignored for rules that are not managed rules. You can check whether a
-- rule is a managed rule by using @DescribeRule@ or @ListRules@ and
-- checking the @ManagedBy@ field of the response.
--
-- 'eventBusName', 'deleteRule_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'name', 'deleteRule_name' - The name of the rule.
newDeleteRule ::
  -- | 'name'
  Core.Text ->
  DeleteRule
newDeleteRule pName_ =
  DeleteRule'
    { force = Core.Nothing,
      eventBusName = Core.Nothing,
      name = pName_
    }

-- | If this is a managed rule, created by an AWS service on your behalf, you
-- must specify @Force@ as @True@ to delete the rule. This parameter is
-- ignored for rules that are not managed rules. You can check whether a
-- rule is a managed rule by using @DescribeRule@ or @ListRules@ and
-- checking the @ManagedBy@ field of the response.
deleteRule_force :: Lens.Lens' DeleteRule (Core.Maybe Core.Bool)
deleteRule_force = Lens.lens (\DeleteRule' {force} -> force) (\s@DeleteRule' {} a -> s {force = a} :: DeleteRule)

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
deleteRule_eventBusName :: Lens.Lens' DeleteRule (Core.Maybe Core.Text)
deleteRule_eventBusName = Lens.lens (\DeleteRule' {eventBusName} -> eventBusName) (\s@DeleteRule' {} a -> s {eventBusName = a} :: DeleteRule)

-- | The name of the rule.
deleteRule_name :: Lens.Lens' DeleteRule Core.Text
deleteRule_name = Lens.lens (\DeleteRule' {name} -> name) (\s@DeleteRule' {} a -> s {name = a} :: DeleteRule)

instance Core.AWSRequest DeleteRule where
  type AWSResponse DeleteRule = DeleteRuleResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteRuleResponse'

instance Core.Hashable DeleteRule

instance Core.NFData DeleteRule

instance Core.ToHeaders DeleteRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DeleteRule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRule where
  toJSON DeleteRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Force" Core..=) Core.<$> force,
            ("EventBusName" Core..=) Core.<$> eventBusName,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath DeleteRule where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRuleResponse ::
  DeleteRuleResponse
newDeleteRuleResponse = DeleteRuleResponse'

instance Core.NFData DeleteRuleResponse
