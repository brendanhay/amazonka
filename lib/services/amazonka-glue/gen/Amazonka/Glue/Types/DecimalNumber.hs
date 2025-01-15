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
-- Module      : Amazonka.Glue.Types.DecimalNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DecimalNumber where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a numeric value in decimal format.
--
-- /See:/ 'newDecimalNumber' smart constructor.
data DecimalNumber = DecimalNumber'
  { -- | The unscaled numeric value.
    unscaledValue :: Data.Base64,
    -- | The scale that determines where the decimal point falls in the unscaled
    -- value.
    scale :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecimalNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unscaledValue', 'decimalNumber_unscaledValue' - The unscaled numeric value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'scale', 'decimalNumber_scale' - The scale that determines where the decimal point falls in the unscaled
-- value.
newDecimalNumber ::
  -- | 'unscaledValue'
  Prelude.ByteString ->
  -- | 'scale'
  Prelude.Int ->
  DecimalNumber
newDecimalNumber pUnscaledValue_ pScale_ =
  DecimalNumber'
    { unscaledValue =
        Data._Base64 Lens.# pUnscaledValue_,
      scale = pScale_
    }

-- | The unscaled numeric value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
decimalNumber_unscaledValue :: Lens.Lens' DecimalNumber Prelude.ByteString
decimalNumber_unscaledValue = Lens.lens (\DecimalNumber' {unscaledValue} -> unscaledValue) (\s@DecimalNumber' {} a -> s {unscaledValue = a} :: DecimalNumber) Prelude.. Data._Base64

-- | The scale that determines where the decimal point falls in the unscaled
-- value.
decimalNumber_scale :: Lens.Lens' DecimalNumber Prelude.Int
decimalNumber_scale = Lens.lens (\DecimalNumber' {scale} -> scale) (\s@DecimalNumber' {} a -> s {scale = a} :: DecimalNumber)

instance Data.FromJSON DecimalNumber where
  parseJSON =
    Data.withObject
      "DecimalNumber"
      ( \x ->
          DecimalNumber'
            Prelude.<$> (x Data..: "UnscaledValue")
            Prelude.<*> (x Data..: "Scale")
      )

instance Prelude.Hashable DecimalNumber where
  hashWithSalt _salt DecimalNumber' {..} =
    _salt
      `Prelude.hashWithSalt` unscaledValue
      `Prelude.hashWithSalt` scale

instance Prelude.NFData DecimalNumber where
  rnf DecimalNumber' {..} =
    Prelude.rnf unscaledValue `Prelude.seq`
      Prelude.rnf scale

instance Data.ToJSON DecimalNumber where
  toJSON DecimalNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("UnscaledValue" Data..= unscaledValue),
            Prelude.Just ("Scale" Data..= scale)
          ]
      )
