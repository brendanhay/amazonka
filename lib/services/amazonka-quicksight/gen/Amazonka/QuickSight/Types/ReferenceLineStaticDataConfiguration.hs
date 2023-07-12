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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineStaticDataConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineStaticDataConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The static data configuration of the reference line data configuration.
--
-- /See:/ 'newReferenceLineStaticDataConfiguration' smart constructor.
data ReferenceLineStaticDataConfiguration = ReferenceLineStaticDataConfiguration'
  { -- | The double input of the static data.
    value :: Data.Sensitive Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceLineStaticDataConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'referenceLineStaticDataConfiguration_value' - The double input of the static data.
newReferenceLineStaticDataConfiguration ::
  -- | 'value'
  Prelude.Double ->
  ReferenceLineStaticDataConfiguration
newReferenceLineStaticDataConfiguration pValue_ =
  ReferenceLineStaticDataConfiguration'
    { value =
        Data._Sensitive Lens.# pValue_
    }

-- | The double input of the static data.
referenceLineStaticDataConfiguration_value :: Lens.Lens' ReferenceLineStaticDataConfiguration Prelude.Double
referenceLineStaticDataConfiguration_value = Lens.lens (\ReferenceLineStaticDataConfiguration' {value} -> value) (\s@ReferenceLineStaticDataConfiguration' {} a -> s {value = a} :: ReferenceLineStaticDataConfiguration) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    ReferenceLineStaticDataConfiguration
  where
  parseJSON =
    Data.withObject
      "ReferenceLineStaticDataConfiguration"
      ( \x ->
          ReferenceLineStaticDataConfiguration'
            Prelude.<$> (x Data..: "Value")
      )

instance
  Prelude.Hashable
    ReferenceLineStaticDataConfiguration
  where
  hashWithSalt
    _salt
    ReferenceLineStaticDataConfiguration' {..} =
      _salt `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    ReferenceLineStaticDataConfiguration
  where
  rnf ReferenceLineStaticDataConfiguration' {..} =
    Prelude.rnf value

instance
  Data.ToJSON
    ReferenceLineStaticDataConfiguration
  where
  toJSON ReferenceLineStaticDataConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Value" Data..= value)]
      )
