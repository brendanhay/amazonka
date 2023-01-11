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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ResourcePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource policy grants one or more Amazon Web Services services and
-- accounts permissions to access X-Ray. Each resource policy is associated
-- with a specific Amazon Web Services account.
--
-- /See:/ 'newResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | When the policy was last updated, in Unix time seconds.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The resource policy document, which can be up to 5kb in size.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource policy. Must be unique within a specific Amazon
    -- Web Services account.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | Returns the current policy revision id for this policy name.
    policyRevisionId :: Prelude.Maybe Prelude.Text
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
-- 'lastUpdatedTime', 'resourcePolicy_lastUpdatedTime' - When the policy was last updated, in Unix time seconds.
--
-- 'policyDocument', 'resourcePolicy_policyDocument' - The resource policy document, which can be up to 5kb in size.
--
-- 'policyName', 'resourcePolicy_policyName' - The name of the resource policy. Must be unique within a specific Amazon
-- Web Services account.
--
-- 'policyRevisionId', 'resourcePolicy_policyRevisionId' - Returns the current policy revision id for this policy name.
newResourcePolicy ::
  ResourcePolicy
newResourcePolicy =
  ResourcePolicy'
    { lastUpdatedTime = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      policyName = Prelude.Nothing,
      policyRevisionId = Prelude.Nothing
    }

-- | When the policy was last updated, in Unix time seconds.
resourcePolicy_lastUpdatedTime :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.UTCTime)
resourcePolicy_lastUpdatedTime = Lens.lens (\ResourcePolicy' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourcePolicy' {} a -> s {lastUpdatedTime = a} :: ResourcePolicy) Prelude.. Lens.mapping Data._Time

-- | The resource policy document, which can be up to 5kb in size.
resourcePolicy_policyDocument :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policyDocument = Lens.lens (\ResourcePolicy' {policyDocument} -> policyDocument) (\s@ResourcePolicy' {} a -> s {policyDocument = a} :: ResourcePolicy)

-- | The name of the resource policy. Must be unique within a specific Amazon
-- Web Services account.
resourcePolicy_policyName :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policyName = Lens.lens (\ResourcePolicy' {policyName} -> policyName) (\s@ResourcePolicy' {} a -> s {policyName = a} :: ResourcePolicy)

-- | Returns the current policy revision id for this policy name.
resourcePolicy_policyRevisionId :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_policyRevisionId = Lens.lens (\ResourcePolicy' {policyRevisionId} -> policyRevisionId) (\s@ResourcePolicy' {} a -> s {policyRevisionId = a} :: ResourcePolicy)

instance Data.FromJSON ResourcePolicy where
  parseJSON =
    Data.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Prelude.<$> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "PolicyDocument")
            Prelude.<*> (x Data..:? "PolicyName")
            Prelude.<*> (x Data..:? "PolicyRevisionId")
      )

instance Prelude.Hashable ResourcePolicy where
  hashWithSalt _salt ResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyRevisionId

instance Prelude.NFData ResourcePolicy where
  rnf ResourcePolicy' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyRevisionId
