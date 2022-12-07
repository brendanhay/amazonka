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
-- Module      : Amazonka.IoT.Types.EffectivePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.EffectivePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON EffectivePolicy where
  parseJSON =
    Data.withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            Prelude.<$> (x Data..:? "policyName")
            Prelude.<*> (x Data..:? "policyDocument")
            Prelude.<*> (x Data..:? "policyArn")
      )

instance Prelude.Hashable EffectivePolicy where
  hashWithSalt _salt EffectivePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData EffectivePolicy where
  rnf EffectivePolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyArn
