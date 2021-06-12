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
-- Module      : Network.AWS.CloudWatchLogs.Types.ResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ResourcePolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A policy enabling one or more entities to put logs to a log group in
-- this account.
--
-- /See:/ 'newResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | The name of the resource policy.
    policyName :: Core.Maybe Core.Text,
    -- | The details of the policy.
    policyDocument :: Core.Maybe Core.Text,
    -- | Timestamp showing when this policy was last updated, expressed as the
    -- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    lastUpdatedTime :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'resourcePolicy_policyName' - The name of the resource policy.
--
-- 'policyDocument', 'resourcePolicy_policyDocument' - The details of the policy.
--
-- 'lastUpdatedTime', 'resourcePolicy_lastUpdatedTime' - Timestamp showing when this policy was last updated, expressed as the
-- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
newResourcePolicy ::
  ResourcePolicy
newResourcePolicy =
  ResourcePolicy'
    { policyName = Core.Nothing,
      policyDocument = Core.Nothing,
      lastUpdatedTime = Core.Nothing
    }

-- | The name of the resource policy.
resourcePolicy_policyName :: Lens.Lens' ResourcePolicy (Core.Maybe Core.Text)
resourcePolicy_policyName = Lens.lens (\ResourcePolicy' {policyName} -> policyName) (\s@ResourcePolicy' {} a -> s {policyName = a} :: ResourcePolicy)

-- | The details of the policy.
resourcePolicy_policyDocument :: Lens.Lens' ResourcePolicy (Core.Maybe Core.Text)
resourcePolicy_policyDocument = Lens.lens (\ResourcePolicy' {policyDocument} -> policyDocument) (\s@ResourcePolicy' {} a -> s {policyDocument = a} :: ResourcePolicy)

-- | Timestamp showing when this policy was last updated, expressed as the
-- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
resourcePolicy_lastUpdatedTime :: Lens.Lens' ResourcePolicy (Core.Maybe Core.Natural)
resourcePolicy_lastUpdatedTime = Lens.lens (\ResourcePolicy' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourcePolicy' {} a -> s {lastUpdatedTime = a} :: ResourcePolicy)

instance Core.FromJSON ResourcePolicy where
  parseJSON =
    Core.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Core.<$> (x Core..:? "policyName")
            Core.<*> (x Core..:? "policyDocument")
            Core.<*> (x Core..:? "lastUpdatedTime")
      )

instance Core.Hashable ResourcePolicy

instance Core.NFData ResourcePolicy
