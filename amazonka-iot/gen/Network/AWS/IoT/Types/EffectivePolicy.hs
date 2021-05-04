{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The policy that has the effect on the authorization results.
--
-- /See:/ 'newEffectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { -- | The policy name.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The IAM policy document.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The policy ARN.
    policyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { policyName = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      policyArn = Prelude.Nothing
    }

-- | The policy name.
effectivePolicy_policyName :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.Text)
effectivePolicy_policyName = Lens.lens (\EffectivePolicy' {policyName} -> policyName) (\s@EffectivePolicy' {} a -> s {policyName = a} :: EffectivePolicy)

-- | The IAM policy document.
effectivePolicy_policyDocument :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.Text)
effectivePolicy_policyDocument = Lens.lens (\EffectivePolicy' {policyDocument} -> policyDocument) (\s@EffectivePolicy' {} a -> s {policyDocument = a} :: EffectivePolicy)

-- | The policy ARN.
effectivePolicy_policyArn :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.Text)
effectivePolicy_policyArn = Lens.lens (\EffectivePolicy' {policyArn} -> policyArn) (\s@EffectivePolicy' {} a -> s {policyArn = a} :: EffectivePolicy)

instance Prelude.FromJSON EffectivePolicy where
  parseJSON =
    Prelude.withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            Prelude.<$> (x Prelude..:? "policyName")
            Prelude.<*> (x Prelude..:? "policyDocument")
            Prelude.<*> (x Prelude..:? "policyArn")
      )

instance Prelude.Hashable EffectivePolicy

instance Prelude.NFData EffectivePolicy
