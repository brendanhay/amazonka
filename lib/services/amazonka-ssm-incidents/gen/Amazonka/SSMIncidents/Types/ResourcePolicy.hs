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
-- Module      : Amazonka.SSMIncidents.Types.ResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ResourcePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The resource policy that allows Incident Manager to perform actions on
-- resources on your behalf.
--
-- /See:/ 'newResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | The JSON blob that describes the policy.
    policyDocument :: Prelude.Text,
    -- | The ID of the resource policy.
    policyId :: Prelude.Text,
    -- | The Amazon Web Services Region that policy allows resources to be used
    -- in.
    ramResourceShareRegion :: Prelude.Text
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
-- 'policyDocument', 'resourcePolicy_policyDocument' - The JSON blob that describes the policy.
--
-- 'policyId', 'resourcePolicy_policyId' - The ID of the resource policy.
--
-- 'ramResourceShareRegion', 'resourcePolicy_ramResourceShareRegion' - The Amazon Web Services Region that policy allows resources to be used
-- in.
newResourcePolicy ::
  -- | 'policyDocument'
  Prelude.Text ->
  -- | 'policyId'
  Prelude.Text ->
  -- | 'ramResourceShareRegion'
  Prelude.Text ->
  ResourcePolicy
newResourcePolicy
  pPolicyDocument_
  pPolicyId_
  pRamResourceShareRegion_ =
    ResourcePolicy'
      { policyDocument = pPolicyDocument_,
        policyId = pPolicyId_,
        ramResourceShareRegion = pRamResourceShareRegion_
      }

-- | The JSON blob that describes the policy.
resourcePolicy_policyDocument :: Lens.Lens' ResourcePolicy Prelude.Text
resourcePolicy_policyDocument = Lens.lens (\ResourcePolicy' {policyDocument} -> policyDocument) (\s@ResourcePolicy' {} a -> s {policyDocument = a} :: ResourcePolicy)

-- | The ID of the resource policy.
resourcePolicy_policyId :: Lens.Lens' ResourcePolicy Prelude.Text
resourcePolicy_policyId = Lens.lens (\ResourcePolicy' {policyId} -> policyId) (\s@ResourcePolicy' {} a -> s {policyId = a} :: ResourcePolicy)

-- | The Amazon Web Services Region that policy allows resources to be used
-- in.
resourcePolicy_ramResourceShareRegion :: Lens.Lens' ResourcePolicy Prelude.Text
resourcePolicy_ramResourceShareRegion = Lens.lens (\ResourcePolicy' {ramResourceShareRegion} -> ramResourceShareRegion) (\s@ResourcePolicy' {} a -> s {ramResourceShareRegion = a} :: ResourcePolicy)

instance Core.FromJSON ResourcePolicy where
  parseJSON =
    Core.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Prelude.<$> (x Core..: "policyDocument")
            Prelude.<*> (x Core..: "policyId")
            Prelude.<*> (x Core..: "ramResourceShareRegion")
      )

instance Prelude.Hashable ResourcePolicy where
  hashWithSalt _salt ResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` ramResourceShareRegion

instance Prelude.NFData ResourcePolicy where
  rnf ResourcePolicy' {..} =
    Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf ramResourceShareRegion
