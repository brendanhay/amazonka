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
-- Module      : Amazonka.LexV2Models.Types.SampleValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SampleValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines one of the values for a slot type.
--
-- /See:/ 'newSampleValue' smart constructor.
data SampleValue = SampleValue'
  { -- | The value that can be used for a slot type.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SampleValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'sampleValue_value' - The value that can be used for a slot type.
newSampleValue ::
  -- | 'value'
  Prelude.Text ->
  SampleValue
newSampleValue pValue_ =
  SampleValue' {value = pValue_}

-- | The value that can be used for a slot type.
sampleValue_value :: Lens.Lens' SampleValue Prelude.Text
sampleValue_value = Lens.lens (\SampleValue' {value} -> value) (\s@SampleValue' {} a -> s {value = a} :: SampleValue)

instance Data.FromJSON SampleValue where
  parseJSON =
    Data.withObject
      "SampleValue"
      (\x -> SampleValue' Prelude.<$> (x Data..: "value"))

instance Prelude.Hashable SampleValue where
  hashWithSalt _salt SampleValue' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData SampleValue where
  rnf SampleValue' {..} = Prelude.rnf value

instance Data.ToJSON SampleValue where
  toJSON SampleValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("value" Data..= value)]
      )
