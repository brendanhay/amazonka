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
-- Module      : Amazonka.AppFlow.Types.DynatraceSourceProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DynatraceSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Dynatrace is being used as a
-- source.
--
-- /See:/ 'newDynatraceSourceProperties' smart constructor.
data DynatraceSourceProperties = DynatraceSourceProperties'
  { -- | The object specified in the Dynatrace flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynatraceSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'dynatraceSourceProperties_object' - The object specified in the Dynatrace flow source.
newDynatraceSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  DynatraceSourceProperties
newDynatraceSourceProperties pObject_ =
  DynatraceSourceProperties' {object' = pObject_}

-- | The object specified in the Dynatrace flow source.
dynatraceSourceProperties_object :: Lens.Lens' DynatraceSourceProperties Prelude.Text
dynatraceSourceProperties_object = Lens.lens (\DynatraceSourceProperties' {object'} -> object') (\s@DynatraceSourceProperties' {} a -> s {object' = a} :: DynatraceSourceProperties)

instance Data.FromJSON DynatraceSourceProperties where
  parseJSON =
    Data.withObject
      "DynatraceSourceProperties"
      ( \x ->
          DynatraceSourceProperties'
            Prelude.<$> (x Data..: "object")
      )

instance Prelude.Hashable DynatraceSourceProperties where
  hashWithSalt _salt DynatraceSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData DynatraceSourceProperties where
  rnf DynatraceSourceProperties' {..} =
    Prelude.rnf object'

instance Data.ToJSON DynatraceSourceProperties where
  toJSON DynatraceSourceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Data..= object')]
      )
