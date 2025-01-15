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
-- Module      : Amazonka.CloudWatchEvents.RemoveTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified targets from the specified rule. When the rule is
-- triggered, those targets are no longer be invoked.
--
-- A successful execution of @RemoveTargets@ doesn\'t guarantee all targets
-- are removed from the rule, it means that the target(s) listed in the
-- request are removed.
--
-- When you remove a target, when the associated rule triggers, removed
-- targets might continue to be invoked. Allow a short period of time for
-- changes to take effect.
--
-- This action can partially fail if too many requests are made at the same
-- time. If that happens, @FailedEntryCount@ is non-zero in the response
-- and each entry in @FailedEntries@ provides the ID of the failed target
-- and the error code.
module Amazonka.CloudWatchEvents.RemoveTargets
  ( -- * Creating a Request
    RemoveTargets (..),
    newRemoveTargets,

    -- * Request Lenses
    removeTargets_eventBusName,
    removeTargets_force,
    removeTargets_rule,
    removeTargets_ids,

    -- * Destructuring the Response
    RemoveTargetsResponse (..),
    newRemoveTargetsResponse,

    -- * Response Lenses
    removeTargetsResponse_failedEntries,
    removeTargetsResponse_failedEntryCount,
    removeTargetsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveTargets' smart constructor.
data RemoveTargets = RemoveTargets'
  { -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | If this is a managed rule, created by an Amazon Web Services service on
    -- your behalf, you must specify @Force@ as @True@ to remove targets. This
    -- parameter is ignored for rules that are not managed rules. You can check
    -- whether a rule is a managed rule by using @DescribeRule@ or @ListRules@
    -- and checking the @ManagedBy@ field of the response.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The name of the rule.
    rule :: Prelude.Text,
    -- | The IDs of the targets to remove from the rule.
    ids :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusName', 'removeTargets_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'force', 'removeTargets_force' - If this is a managed rule, created by an Amazon Web Services service on
-- your behalf, you must specify @Force@ as @True@ to remove targets. This
-- parameter is ignored for rules that are not managed rules. You can check
-- whether a rule is a managed rule by using @DescribeRule@ or @ListRules@
-- and checking the @ManagedBy@ field of the response.
--
-- 'rule', 'removeTargets_rule' - The name of the rule.
--
-- 'ids', 'removeTargets_ids' - The IDs of the targets to remove from the rule.
newRemoveTargets ::
  -- | 'rule'
  Prelude.Text ->
  -- | 'ids'
  Prelude.NonEmpty Prelude.Text ->
  RemoveTargets
newRemoveTargets pRule_ pIds_ =
  RemoveTargets'
    { eventBusName = Prelude.Nothing,
      force = Prelude.Nothing,
      rule = pRule_,
      ids = Lens.coerced Lens.# pIds_
    }

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
removeTargets_eventBusName :: Lens.Lens' RemoveTargets (Prelude.Maybe Prelude.Text)
removeTargets_eventBusName = Lens.lens (\RemoveTargets' {eventBusName} -> eventBusName) (\s@RemoveTargets' {} a -> s {eventBusName = a} :: RemoveTargets)

-- | If this is a managed rule, created by an Amazon Web Services service on
-- your behalf, you must specify @Force@ as @True@ to remove targets. This
-- parameter is ignored for rules that are not managed rules. You can check
-- whether a rule is a managed rule by using @DescribeRule@ or @ListRules@
-- and checking the @ManagedBy@ field of the response.
removeTargets_force :: Lens.Lens' RemoveTargets (Prelude.Maybe Prelude.Bool)
removeTargets_force = Lens.lens (\RemoveTargets' {force} -> force) (\s@RemoveTargets' {} a -> s {force = a} :: RemoveTargets)

-- | The name of the rule.
removeTargets_rule :: Lens.Lens' RemoveTargets Prelude.Text
removeTargets_rule = Lens.lens (\RemoveTargets' {rule} -> rule) (\s@RemoveTargets' {} a -> s {rule = a} :: RemoveTargets)

-- | The IDs of the targets to remove from the rule.
removeTargets_ids :: Lens.Lens' RemoveTargets (Prelude.NonEmpty Prelude.Text)
removeTargets_ids = Lens.lens (\RemoveTargets' {ids} -> ids) (\s@RemoveTargets' {} a -> s {ids = a} :: RemoveTargets) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTargets where
  type
    AWSResponse RemoveTargets =
      RemoveTargetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveTargetsResponse'
            Prelude.<$> (x Data..?> "FailedEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "FailedEntryCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTargets where
  hashWithSalt _salt RemoveTargets' {..} =
    _salt
      `Prelude.hashWithSalt` eventBusName
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` rule
      `Prelude.hashWithSalt` ids

instance Prelude.NFData RemoveTargets where
  rnf RemoveTargets' {..} =
    Prelude.rnf eventBusName `Prelude.seq`
      Prelude.rnf force `Prelude.seq`
        Prelude.rnf rule `Prelude.seq`
          Prelude.rnf ids

instance Data.ToHeaders RemoveTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.RemoveTargets" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTargets where
  toJSON RemoveTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventBusName" Data..=) Prelude.<$> eventBusName,
            ("Force" Data..=) Prelude.<$> force,
            Prelude.Just ("Rule" Data..= rule),
            Prelude.Just ("Ids" Data..= ids)
          ]
      )

instance Data.ToPath RemoveTargets where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTargetsResponse' smart constructor.
data RemoveTargetsResponse = RemoveTargetsResponse'
  { -- | The failed target entries.
    failedEntries :: Prelude.Maybe [RemoveTargetsResultEntry],
    -- | The number of failed entries.
    failedEntryCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntries', 'removeTargetsResponse_failedEntries' - The failed target entries.
--
-- 'failedEntryCount', 'removeTargetsResponse_failedEntryCount' - The number of failed entries.
--
-- 'httpStatus', 'removeTargetsResponse_httpStatus' - The response's http status code.
newRemoveTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTargetsResponse
newRemoveTargetsResponse pHttpStatus_ =
  RemoveTargetsResponse'
    { failedEntries =
        Prelude.Nothing,
      failedEntryCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The failed target entries.
removeTargetsResponse_failedEntries :: Lens.Lens' RemoveTargetsResponse (Prelude.Maybe [RemoveTargetsResultEntry])
removeTargetsResponse_failedEntries = Lens.lens (\RemoveTargetsResponse' {failedEntries} -> failedEntries) (\s@RemoveTargetsResponse' {} a -> s {failedEntries = a} :: RemoveTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of failed entries.
removeTargetsResponse_failedEntryCount :: Lens.Lens' RemoveTargetsResponse (Prelude.Maybe Prelude.Int)
removeTargetsResponse_failedEntryCount = Lens.lens (\RemoveTargetsResponse' {failedEntryCount} -> failedEntryCount) (\s@RemoveTargetsResponse' {} a -> s {failedEntryCount = a} :: RemoveTargetsResponse)

-- | The response's http status code.
removeTargetsResponse_httpStatus :: Lens.Lens' RemoveTargetsResponse Prelude.Int
removeTargetsResponse_httpStatus = Lens.lens (\RemoveTargetsResponse' {httpStatus} -> httpStatus) (\s@RemoveTargetsResponse' {} a -> s {httpStatus = a} :: RemoveTargetsResponse)

instance Prelude.NFData RemoveTargetsResponse where
  rnf RemoveTargetsResponse' {..} =
    Prelude.rnf failedEntries `Prelude.seq`
      Prelude.rnf failedEntryCount `Prelude.seq`
        Prelude.rnf httpStatus
