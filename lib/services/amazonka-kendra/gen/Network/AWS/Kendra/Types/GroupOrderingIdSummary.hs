{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kendra.Types.GroupOrderingIdSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.GroupOrderingIdSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.PrincipalMappingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information on the processing of @PUT@ and @DELETE@ actions for mapping
-- users to their groups.
--
-- /See:/ 'newGroupOrderingIdSummary' smart constructor.
data GroupOrderingIdSummary = GroupOrderingIdSummary'
  { -- | The current processing status of actions for mapping users to their
    -- groups. The status can be either @PROCESSING@, @SUCCEEDED@, @DELETING@,
    -- @DELETED@, or @FAILED@.
    status :: Prelude.Maybe PrincipalMappingStatus,
    -- | The reason an action could not be processed. An action can be a @PUT@ or
    -- @DELETE@ action for mapping users to their groups.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The last date-time an action was updated. An action can be a @PUT@ or
    -- @DELETE@ action for mapping users to their groups.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The date-time an action was received by Amazon Kendra. An action can be
    -- a @PUT@ or @DELETE@ action for mapping users to their groups.
    receivedAt :: Prelude.Maybe Core.POSIX,
    -- | The order in which actions should complete processing. An action can be
    -- a @PUT@ or @DELETE@ action for mapping users to their groups.
    orderingId :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupOrderingIdSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'groupOrderingIdSummary_status' - The current processing status of actions for mapping users to their
-- groups. The status can be either @PROCESSING@, @SUCCEEDED@, @DELETING@,
-- @DELETED@, or @FAILED@.
--
-- 'failureReason', 'groupOrderingIdSummary_failureReason' - The reason an action could not be processed. An action can be a @PUT@ or
-- @DELETE@ action for mapping users to their groups.
--
-- 'lastUpdatedAt', 'groupOrderingIdSummary_lastUpdatedAt' - The last date-time an action was updated. An action can be a @PUT@ or
-- @DELETE@ action for mapping users to their groups.
--
-- 'receivedAt', 'groupOrderingIdSummary_receivedAt' - The date-time an action was received by Amazon Kendra. An action can be
-- a @PUT@ or @DELETE@ action for mapping users to their groups.
--
-- 'orderingId', 'groupOrderingIdSummary_orderingId' - The order in which actions should complete processing. An action can be
-- a @PUT@ or @DELETE@ action for mapping users to their groups.
newGroupOrderingIdSummary ::
  GroupOrderingIdSummary
newGroupOrderingIdSummary =
  GroupOrderingIdSummary'
    { status = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      receivedAt = Prelude.Nothing,
      orderingId = Prelude.Nothing
    }

-- | The current processing status of actions for mapping users to their
-- groups. The status can be either @PROCESSING@, @SUCCEEDED@, @DELETING@,
-- @DELETED@, or @FAILED@.
groupOrderingIdSummary_status :: Lens.Lens' GroupOrderingIdSummary (Prelude.Maybe PrincipalMappingStatus)
groupOrderingIdSummary_status = Lens.lens (\GroupOrderingIdSummary' {status} -> status) (\s@GroupOrderingIdSummary' {} a -> s {status = a} :: GroupOrderingIdSummary)

-- | The reason an action could not be processed. An action can be a @PUT@ or
-- @DELETE@ action for mapping users to their groups.
groupOrderingIdSummary_failureReason :: Lens.Lens' GroupOrderingIdSummary (Prelude.Maybe Prelude.Text)
groupOrderingIdSummary_failureReason = Lens.lens (\GroupOrderingIdSummary' {failureReason} -> failureReason) (\s@GroupOrderingIdSummary' {} a -> s {failureReason = a} :: GroupOrderingIdSummary)

-- | The last date-time an action was updated. An action can be a @PUT@ or
-- @DELETE@ action for mapping users to their groups.
groupOrderingIdSummary_lastUpdatedAt :: Lens.Lens' GroupOrderingIdSummary (Prelude.Maybe Prelude.UTCTime)
groupOrderingIdSummary_lastUpdatedAt = Lens.lens (\GroupOrderingIdSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@GroupOrderingIdSummary' {} a -> s {lastUpdatedAt = a} :: GroupOrderingIdSummary) Prelude.. Lens.mapping Core._Time

-- | The date-time an action was received by Amazon Kendra. An action can be
-- a @PUT@ or @DELETE@ action for mapping users to their groups.
groupOrderingIdSummary_receivedAt :: Lens.Lens' GroupOrderingIdSummary (Prelude.Maybe Prelude.UTCTime)
groupOrderingIdSummary_receivedAt = Lens.lens (\GroupOrderingIdSummary' {receivedAt} -> receivedAt) (\s@GroupOrderingIdSummary' {} a -> s {receivedAt = a} :: GroupOrderingIdSummary) Prelude.. Lens.mapping Core._Time

-- | The order in which actions should complete processing. An action can be
-- a @PUT@ or @DELETE@ action for mapping users to their groups.
groupOrderingIdSummary_orderingId :: Lens.Lens' GroupOrderingIdSummary (Prelude.Maybe Prelude.Natural)
groupOrderingIdSummary_orderingId = Lens.lens (\GroupOrderingIdSummary' {orderingId} -> orderingId) (\s@GroupOrderingIdSummary' {} a -> s {orderingId = a} :: GroupOrderingIdSummary)

instance Core.FromJSON GroupOrderingIdSummary where
  parseJSON =
    Core.withObject
      "GroupOrderingIdSummary"
      ( \x ->
          GroupOrderingIdSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "LastUpdatedAt")
            Prelude.<*> (x Core..:? "ReceivedAt")
            Prelude.<*> (x Core..:? "OrderingId")
      )

instance Prelude.Hashable GroupOrderingIdSummary

instance Prelude.NFData GroupOrderingIdSummary
