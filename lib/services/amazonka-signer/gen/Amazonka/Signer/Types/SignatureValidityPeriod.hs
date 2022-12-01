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
-- Module      : Amazonka.Signer.Types.SignatureValidityPeriod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SignatureValidityPeriod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.ValidityType

-- | The validity period for a signing job.
--
-- /See:/ 'newSignatureValidityPeriod' smart constructor.
data SignatureValidityPeriod = SignatureValidityPeriod'
  { -- | The time unit for signature validity.
    type' :: Prelude.Maybe ValidityType,
    -- | The numerical value of the time unit for signature validity.
    value :: Prelude.Maybe Prelude.Int
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
-- 'type'', 'signatureValidityPeriod_type' - The time unit for signature validity.
--
-- 'value', 'signatureValidityPeriod_value' - The numerical value of the time unit for signature validity.
newSignatureValidityPeriod ::
  SignatureValidityPeriod
newSignatureValidityPeriod =
  SignatureValidityPeriod'
    { type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The time unit for signature validity.
signatureValidityPeriod_type :: Lens.Lens' SignatureValidityPeriod (Prelude.Maybe ValidityType)
signatureValidityPeriod_type = Lens.lens (\SignatureValidityPeriod' {type'} -> type') (\s@SignatureValidityPeriod' {} a -> s {type' = a} :: SignatureValidityPeriod)

-- | The numerical value of the time unit for signature validity.
signatureValidityPeriod_value :: Lens.Lens' SignatureValidityPeriod (Prelude.Maybe Prelude.Int)
signatureValidityPeriod_value = Lens.lens (\SignatureValidityPeriod' {value} -> value) (\s@SignatureValidityPeriod' {} a -> s {value = a} :: SignatureValidityPeriod)

instance Core.FromJSON SignatureValidityPeriod where
  parseJSON =
    Core.withObject
      "SignatureValidityPeriod"
      ( \x ->
          SignatureValidityPeriod'
            Prelude.<$> (x Core..:? "type") Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable SignatureValidityPeriod where
  hashWithSalt _salt SignatureValidityPeriod' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData SignatureValidityPeriod where
  rnf SignatureValidityPeriod' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Core.ToJSON SignatureValidityPeriod where
  toJSON SignatureValidityPeriod' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("type" Core..=) Prelude.<$> type',
            ("value" Core..=) Prelude.<$> value
          ]
      )
