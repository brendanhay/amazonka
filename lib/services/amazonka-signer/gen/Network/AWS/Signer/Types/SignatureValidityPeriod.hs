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
-- Module      : Network.AWS.Signer.Types.SignatureValidityPeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Signer.Types.SignatureValidityPeriod where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Signer.Types.ValidityType

-- | The validity period for a signing job.
--
-- /See:/ 'newSignatureValidityPeriod' smart constructor.
data SignatureValidityPeriod = SignatureValidityPeriod'
  { -- | The numerical value of the time unit for signature validity.
    value :: Prelude.Maybe Prelude.Int,
    -- | The time unit for signature validity.
    type' :: Prelude.Maybe ValidityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignatureValidityPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'signatureValidityPeriod_value' - The numerical value of the time unit for signature validity.
--
-- 'type'', 'signatureValidityPeriod_type' - The time unit for signature validity.
newSignatureValidityPeriod ::
  SignatureValidityPeriod
newSignatureValidityPeriod =
  SignatureValidityPeriod'
    { value = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The numerical value of the time unit for signature validity.
signatureValidityPeriod_value :: Lens.Lens' SignatureValidityPeriod (Prelude.Maybe Prelude.Int)
signatureValidityPeriod_value = Lens.lens (\SignatureValidityPeriod' {value} -> value) (\s@SignatureValidityPeriod' {} a -> s {value = a} :: SignatureValidityPeriod)

-- | The time unit for signature validity.
signatureValidityPeriod_type :: Lens.Lens' SignatureValidityPeriod (Prelude.Maybe ValidityType)
signatureValidityPeriod_type = Lens.lens (\SignatureValidityPeriod' {type'} -> type') (\s@SignatureValidityPeriod' {} a -> s {type' = a} :: SignatureValidityPeriod)

instance Core.FromJSON SignatureValidityPeriod where
  parseJSON =
    Core.withObject
      "SignatureValidityPeriod"
      ( \x ->
          SignatureValidityPeriod'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable SignatureValidityPeriod

instance Prelude.NFData SignatureValidityPeriod

instance Core.ToJSON SignatureValidityPeriod where
  toJSON SignatureValidityPeriod' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("type" Core..=) Prelude.<$> type'
          ]
      )
