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
-- Module      : Amazonka.CloudWatchLogs.Types.ResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.ResourcePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A policy enabling one or more entities to put logs to a log group in
-- this account.
--
-- /See:/ 'newResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | The name of the resource policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | Timestamp showing when this policy was last updated, expressed as the
    -- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    lastUpdatedTime :: Prelude.Maybe Prelude.Natural,
    -- | The details of the policy.
    policyDocument :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'lastUpdatedTime', 'resourcePolicy_lastUpdatedTime' - Timestamp showing when this policy was last updated, expressed as the
-- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'policyDocument', 'resourcePolicy_policyDocument' - The details of the policy.
newResourcePolicy ::
  ResourcePolicy
newResourcePolicy =
  ResourcePolicy'
    { policyName = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      policyDocument = Prelude.Nothing
    }

-- | The name of the resource policy.
resourcePolicy_policyName :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policyName = Lens.lens (\ResourcePolicy' {policyName} -> policyName) (\s@ResourcePolicy' {} a -> s {policyName = a} :: ResourcePolicy)

-- | Timestamp showing when this policy was last updated, expressed as the
-- number of milliseconds after Jan 1, 1970 00:00:00 UTC.
resourcePolicy_lastUpdatedTime :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Natural)
resourcePolicy_lastUpdatedTime = Lens.lens (\ResourcePolicy' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourcePolicy' {} a -> s {lastUpdatedTime = a} :: ResourcePolicy)

-- | The details of the policy.
resourcePolicy_policyDocument :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policyDocument = Lens.lens (\ResourcePolicy' {policyDocument} -> policyDocument) (\s@ResourcePolicy' {} a -> s {policyDocument = a} :: ResourcePolicy)

instance Data.FromJSON ResourcePolicy where
  parseJSON =
    Data.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Prelude.<$> (x Data..:? "policyName")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "policyDocument")
      )

instance Prelude.Hashable ResourcePolicy where
  hashWithSalt _salt ResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData ResourcePolicy where
  rnf ResourcePolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf policyDocument
