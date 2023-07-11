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
-- Module      : Amazonka.CloudWatchEvents.DisableRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified rule. A disabled rule won\'t match any events,
-- and won\'t self-trigger if it has a schedule expression.
--
-- When you disable a rule, incoming events might continue to match to the
-- disabled rule. Allow a short period of time for changes to take effect.
module Amazonka.CloudWatchEvents.DisableRule
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

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableRule' smart constructor.
data DisableRule = DisableRule'
  { -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DisableRule
newDisableRule pName_ =
  DisableRule'
    { eventBusName = Prelude.Nothing,
      name = pName_
    }

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
disableRule_eventBusName :: Lens.Lens' DisableRule (Prelude.Maybe Prelude.Text)
disableRule_eventBusName = Lens.lens (\DisableRule' {eventBusName} -> eventBusName) (\s@DisableRule' {} a -> s {eventBusName = a} :: DisableRule)

-- | The name of the rule.
disableRule_name :: Lens.Lens' DisableRule Prelude.Text
disableRule_name = Lens.lens (\DisableRule' {name} -> name) (\s@DisableRule' {} a -> s {name = a} :: DisableRule)

instance Core.AWSRequest DisableRule where
  type AWSResponse DisableRule = DisableRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DisableRuleResponse'

instance Prelude.Hashable DisableRule where
  hashWithSalt _salt DisableRule' {..} =
    _salt
      `Prelude.hashWithSalt` eventBusName
      `Prelude.hashWithSalt` name

instance Prelude.NFData DisableRule where
  rnf DisableRule' {..} =
    Prelude.rnf eventBusName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DisableRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DisableRule" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableRule where
  toJSON DisableRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventBusName" Data..=) Prelude.<$> eventBusName,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath DisableRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableRuleResponse' smart constructor.
data DisableRuleResponse = DisableRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableRuleResponse ::
  DisableRuleResponse
newDisableRuleResponse = DisableRuleResponse'

instance Prelude.NFData DisableRuleResponse where
  rnf _ = ()
