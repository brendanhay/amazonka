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
-- Module      : Amazonka.CloudWatchEvents.DeleteRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
-- Before you can delete the rule, you must remove all targets, using
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_RemoveTargets.html RemoveTargets>.
--
-- When you delete a rule, incoming events might continue to match to the
-- deleted rule. Allow a short period of time for changes to take effect.
--
-- If you call delete rule multiple times for the same rule, all calls will
-- succeed. When you call delete rule for a non-existent custom eventbus,
-- @ResourceNotFoundException@ is returned.
--
-- Managed rules are rules created and managed by another Amazon Web
-- Services service on your behalf. These rules are created by those other
-- Amazon Web Services services to support functionality in those services.
-- You can delete these rules using the @Force@ option, but you should do
-- so only if you are sure the other service is not still using that rule.
module Amazonka.CloudWatchEvents.DeleteRule
  ( -- * Creating a Request
    DeleteRule (..),
    newDeleteRule,

    -- * Request Lenses
    deleteRule_eventBusName,
    deleteRule_force,
    deleteRule_name,

    -- * Destructuring the Response
    DeleteRuleResponse (..),
    newDeleteRuleResponse,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | If this is a managed rule, created by an Amazon Web Services service on
    -- your behalf, you must specify @Force@ as @True@ to delete the rule. This
    -- parameter is ignored for rules that are not managed rules. You can check
    -- whether a rule is a managed rule by using @DescribeRule@ or @ListRules@
    -- and checking the @ManagedBy@ field of the response.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The name of the rule.
    name :: Prelude.Text
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
-- 'eventBusName', 'deleteRule_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'force', 'deleteRule_force' - If this is a managed rule, created by an Amazon Web Services service on
-- your behalf, you must specify @Force@ as @True@ to delete the rule. This
-- parameter is ignored for rules that are not managed rules. You can check
-- whether a rule is a managed rule by using @DescribeRule@ or @ListRules@
-- and checking the @ManagedBy@ field of the response.
--
-- 'name', 'deleteRule_name' - The name of the rule.
newDeleteRule ::
  -- | 'name'
  Prelude.Text ->
  DeleteRule
newDeleteRule pName_ =
  DeleteRule'
    { eventBusName = Prelude.Nothing,
      force = Prelude.Nothing,
      name = pName_
    }

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
deleteRule_eventBusName :: Lens.Lens' DeleteRule (Prelude.Maybe Prelude.Text)
deleteRule_eventBusName = Lens.lens (\DeleteRule' {eventBusName} -> eventBusName) (\s@DeleteRule' {} a -> s {eventBusName = a} :: DeleteRule)

-- | If this is a managed rule, created by an Amazon Web Services service on
-- your behalf, you must specify @Force@ as @True@ to delete the rule. This
-- parameter is ignored for rules that are not managed rules. You can check
-- whether a rule is a managed rule by using @DescribeRule@ or @ListRules@
-- and checking the @ManagedBy@ field of the response.
deleteRule_force :: Lens.Lens' DeleteRule (Prelude.Maybe Prelude.Bool)
deleteRule_force = Lens.lens (\DeleteRule' {force} -> force) (\s@DeleteRule' {} a -> s {force = a} :: DeleteRule)

-- | The name of the rule.
deleteRule_name :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_name = Lens.lens (\DeleteRule' {name} -> name) (\s@DeleteRule' {} a -> s {name = a} :: DeleteRule)

instance Core.AWSRequest DeleteRule where
  type AWSResponse DeleteRule = DeleteRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteRuleResponse'

instance Prelude.Hashable DeleteRule where
  hashWithSalt _salt DeleteRule' {..} =
    _salt
      `Prelude.hashWithSalt` eventBusName
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteRule where
  rnf DeleteRule' {..} =
    Prelude.rnf eventBusName `Prelude.seq`
      Prelude.rnf force `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders DeleteRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DeleteRule" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRule where
  toJSON DeleteRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventBusName" Data..=) Prelude.<$> eventBusName,
            ("Force" Data..=) Prelude.<$> force,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath DeleteRule where
  toPath = Prelude.const "/"

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
