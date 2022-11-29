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
-- Module      : Amazonka.XRay.Types.ResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ResourcePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A resource policy grants one or more Amazon Web Services services and
-- accounts permissions to access X-Ray. Each resource policy is associated
-- with a specific Amazon Web Services account.
--
-- /See:/ 'newResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | The name of the resource policy. Must be unique within a specific Amazon
    -- Web Services account.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | When the policy was last updated, in Unix time seconds.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | Returns the current policy revision id for this policy name.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The resource policy document, which can be up to 5kb in size.
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
-- 'policyName', 'resourcePolicy_policyName' - The name of the resource policy. Must be unique within a specific Amazon
-- Web Services account.
--
-- 'lastUpdatedTime', 'resourcePolicy_lastUpdatedTime' - When the policy was last updated, in Unix time seconds.
--
-- 'policyRevisionId', 'resourcePolicy_policyRevisionId' - Returns the current policy revision id for this policy name.
--
-- 'policyDocument', 'resourcePolicy_policyDocument' - The resource policy document, which can be up to 5kb in size.
newResourcePolicy ::
  ResourcePolicy
newResourcePolicy =
  ResourcePolicy'
    { policyName = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      policyRevisionId = Prelude.Nothing,
      policyDocument = Prelude.Nothing
    }

-- | The name of the resource policy. Must be unique within a specific Amazon
-- Web Services account.
resourcePolicy_policyName :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policyName = Lens.lens (\ResourcePolicy' {policyName} -> policyName) (\s@ResourcePolicy' {} a -> s {policyName = a} :: ResourcePolicy)

-- | When the policy was last updated, in Unix time seconds.
resourcePolicy_lastUpdatedTime :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.UTCTime)
resourcePolicy_lastUpdatedTime = Lens.lens (\ResourcePolicy' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourcePolicy' {} a -> s {lastUpdatedTime = a} :: ResourcePolicy) Prelude.. Lens.mapping Core._Time

-- | Returns the current policy revision id for this policy name.
resourcePolicy_policyRevisionId :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policyRevisionId = Lens.lens (\ResourcePolicy' {policyRevisionId} -> policyRevisionId) (\s@ResourcePolicy' {} a -> s {policyRevisionId = a} :: ResourcePolicy)

-- | The resource policy document, which can be up to 5kb in size.
resourcePolicy_policyDocument :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policyDocument = Lens.lens (\ResourcePolicy' {policyDocument} -> policyDocument) (\s@ResourcePolicy' {} a -> s {policyDocument = a} :: ResourcePolicy)

instance Core.FromJSON ResourcePolicy where
  parseJSON =
    Core.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Prelude.<$> (x Core..:? "PolicyName")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "PolicyRevisionId")
            Prelude.<*> (x Core..:? "PolicyDocument")
      )

instance Prelude.Hashable ResourcePolicy where
  hashWithSalt _salt ResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` policyRevisionId
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData ResourcePolicy where
  rnf ResourcePolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf policyDocument
