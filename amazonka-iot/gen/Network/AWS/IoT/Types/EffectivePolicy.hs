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
-- Module      : Network.AWS.IoT.Types.EffectivePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.EffectivePolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The policy that has the effect on the authorization results.
--
-- /See:/ 'newEffectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { -- | The policy name.
    policyName :: Core.Maybe Core.Text,
    -- | The IAM policy document.
    policyDocument :: Core.Maybe Core.Text,
    -- | The policy ARN.
    policyArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EffectivePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'effectivePolicy_policyName' - The policy name.
--
-- 'policyDocument', 'effectivePolicy_policyDocument' - The IAM policy document.
--
-- 'policyArn', 'effectivePolicy_policyArn' - The policy ARN.
newEffectivePolicy ::
  EffectivePolicy
newEffectivePolicy =
  EffectivePolicy'
    { policyName = Core.Nothing,
      policyDocument = Core.Nothing,
      policyArn = Core.Nothing
    }

-- | The policy name.
effectivePolicy_policyName :: Lens.Lens' EffectivePolicy (Core.Maybe Core.Text)
effectivePolicy_policyName = Lens.lens (\EffectivePolicy' {policyName} -> policyName) (\s@EffectivePolicy' {} a -> s {policyName = a} :: EffectivePolicy)

-- | The IAM policy document.
effectivePolicy_policyDocument :: Lens.Lens' EffectivePolicy (Core.Maybe Core.Text)
effectivePolicy_policyDocument = Lens.lens (\EffectivePolicy' {policyDocument} -> policyDocument) (\s@EffectivePolicy' {} a -> s {policyDocument = a} :: EffectivePolicy)

-- | The policy ARN.
effectivePolicy_policyArn :: Lens.Lens' EffectivePolicy (Core.Maybe Core.Text)
effectivePolicy_policyArn = Lens.lens (\EffectivePolicy' {policyArn} -> policyArn) (\s@EffectivePolicy' {} a -> s {policyArn = a} :: EffectivePolicy)

instance Core.FromJSON EffectivePolicy where
  parseJSON =
    Core.withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            Core.<$> (x Core..:? "policyName")
            Core.<*> (x Core..:? "policyDocument")
            Core.<*> (x Core..:? "policyArn")
      )

instance Core.Hashable EffectivePolicy

instance Core.NFData EffectivePolicy
