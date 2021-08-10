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
-- Module      : Network.AWS.CloudWatchEvents.RemoveTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified targets from the specified rule. When the rule is
-- triggered, those targets are no longer be invoked.
--
-- When you remove a target, when the associated rule triggers, removed
-- targets might continue to be invoked. Allow a short period of time for
-- changes to take effect.
--
-- This action can partially fail if too many requests are made at the same
-- time. If that happens, @FailedEntryCount@ is non-zero in the response
-- and each entry in @FailedEntries@ provides the ID of the failed target
-- and the error code.
module Network.AWS.CloudWatchEvents.RemoveTargets
  ( -- * Creating a Request
    RemoveTargets (..),
    newRemoveTargets,

    -- * Request Lenses
    removeTargets_force,
    removeTargets_eventBusName,
    removeTargets_rule,
    removeTargets_ids,

    -- * Destructuring the Response
    RemoveTargetsResponse (..),
    newRemoveTargetsResponse,

    -- * Response Lenses
    removeTargetsResponse_failedEntryCount,
    removeTargetsResponse_failedEntries,
    removeTargetsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveTargets' smart constructor.
data RemoveTargets = RemoveTargets'
  { -- | If this is a managed rule, created by an AWS service on your behalf, you
    -- must specify @Force@ as @True@ to remove targets. This parameter is
    -- ignored for rules that are not managed rules. You can check whether a
    -- rule is a managed rule by using @DescribeRule@ or @ListRules@ and
    -- checking the @ManagedBy@ field of the response.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
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
-- 'force', 'removeTargets_force' - If this is a managed rule, created by an AWS service on your behalf, you
-- must specify @Force@ as @True@ to remove targets. This parameter is
-- ignored for rules that are not managed rules. You can check whether a
-- rule is a managed rule by using @DescribeRule@ or @ListRules@ and
-- checking the @ManagedBy@ field of the response.
--
-- 'eventBusName', 'removeTargets_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
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
    { force = Prelude.Nothing,
      eventBusName = Prelude.Nothing,
      rule = pRule_,
      ids = Lens._Coerce Lens.# pIds_
    }

-- | If this is a managed rule, created by an AWS service on your behalf, you
-- must specify @Force@ as @True@ to remove targets. This parameter is
-- ignored for rules that are not managed rules. You can check whether a
-- rule is a managed rule by using @DescribeRule@ or @ListRules@ and
-- checking the @ManagedBy@ field of the response.
removeTargets_force :: Lens.Lens' RemoveTargets (Prelude.Maybe Prelude.Bool)
removeTargets_force = Lens.lens (\RemoveTargets' {force} -> force) (\s@RemoveTargets' {} a -> s {force = a} :: RemoveTargets)

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
removeTargets_eventBusName :: Lens.Lens' RemoveTargets (Prelude.Maybe Prelude.Text)
removeTargets_eventBusName = Lens.lens (\RemoveTargets' {eventBusName} -> eventBusName) (\s@RemoveTargets' {} a -> s {eventBusName = a} :: RemoveTargets)

-- | The name of the rule.
removeTargets_rule :: Lens.Lens' RemoveTargets Prelude.Text
removeTargets_rule = Lens.lens (\RemoveTargets' {rule} -> rule) (\s@RemoveTargets' {} a -> s {rule = a} :: RemoveTargets)

-- | The IDs of the targets to remove from the rule.
removeTargets_ids :: Lens.Lens' RemoveTargets (Prelude.NonEmpty Prelude.Text)
removeTargets_ids = Lens.lens (\RemoveTargets' {ids} -> ids) (\s@RemoveTargets' {} a -> s {ids = a} :: RemoveTargets) Prelude.. Lens._Coerce

instance Core.AWSRequest RemoveTargets where
  type
    AWSResponse RemoveTargets =
      RemoveTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveTargetsResponse'
            Prelude.<$> (x Core..?> "FailedEntryCount")
            Prelude.<*> (x Core..?> "FailedEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTargets

instance Prelude.NFData RemoveTargets

instance Core.ToHeaders RemoveTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.RemoveTargets" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveTargets where
  toJSON RemoveTargets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Force" Core..=) Prelude.<$> force,
            ("EventBusName" Core..=) Prelude.<$> eventBusName,
            Prelude.Just ("Rule" Core..= rule),
            Prelude.Just ("Ids" Core..= ids)
          ]
      )

instance Core.ToPath RemoveTargets where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTargetsResponse' smart constructor.
data RemoveTargetsResponse = RemoveTargetsResponse'
  { -- | The number of failed entries.
    failedEntryCount :: Prelude.Maybe Prelude.Int,
    -- | The failed target entries.
    failedEntries :: Prelude.Maybe [RemoveTargetsResultEntry],
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
-- 'failedEntryCount', 'removeTargetsResponse_failedEntryCount' - The number of failed entries.
--
-- 'failedEntries', 'removeTargetsResponse_failedEntries' - The failed target entries.
--
-- 'httpStatus', 'removeTargetsResponse_httpStatus' - The response's http status code.
newRemoveTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTargetsResponse
newRemoveTargetsResponse pHttpStatus_ =
  RemoveTargetsResponse'
    { failedEntryCount =
        Prelude.Nothing,
      failedEntries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of failed entries.
removeTargetsResponse_failedEntryCount :: Lens.Lens' RemoveTargetsResponse (Prelude.Maybe Prelude.Int)
removeTargetsResponse_failedEntryCount = Lens.lens (\RemoveTargetsResponse' {failedEntryCount} -> failedEntryCount) (\s@RemoveTargetsResponse' {} a -> s {failedEntryCount = a} :: RemoveTargetsResponse)

-- | The failed target entries.
removeTargetsResponse_failedEntries :: Lens.Lens' RemoveTargetsResponse (Prelude.Maybe [RemoveTargetsResultEntry])
removeTargetsResponse_failedEntries = Lens.lens (\RemoveTargetsResponse' {failedEntries} -> failedEntries) (\s@RemoveTargetsResponse' {} a -> s {failedEntries = a} :: RemoveTargetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
removeTargetsResponse_httpStatus :: Lens.Lens' RemoveTargetsResponse Prelude.Int
removeTargetsResponse_httpStatus = Lens.lens (\RemoveTargetsResponse' {httpStatus} -> httpStatus) (\s@RemoveTargetsResponse' {} a -> s {httpStatus = a} :: RemoveTargetsResponse)

instance Prelude.NFData RemoveTargetsResponse
