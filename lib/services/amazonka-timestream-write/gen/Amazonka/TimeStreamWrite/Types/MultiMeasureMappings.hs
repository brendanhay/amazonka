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
-- Module      : Amazonka.TimeStreamWrite.Types.MultiMeasureMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.MultiMeasureMappings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.MultiMeasureAttributeMapping

-- |
--
-- /See:/ 'newMultiMeasureMappings' smart constructor.
data MultiMeasureMappings = MultiMeasureMappings'
  { targetMultiMeasureName :: Prelude.Maybe Prelude.Text,
    multiMeasureAttributeMappings :: Prelude.NonEmpty MultiMeasureAttributeMapping
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiMeasureMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetMultiMeasureName', 'multiMeasureMappings_targetMultiMeasureName' -
--
-- 'multiMeasureAttributeMappings', 'multiMeasureMappings_multiMeasureAttributeMappings' -
newMultiMeasureMappings ::
  -- | 'multiMeasureAttributeMappings'
  Prelude.NonEmpty MultiMeasureAttributeMapping ->
  MultiMeasureMappings
newMultiMeasureMappings
  pMultiMeasureAttributeMappings_ =
    MultiMeasureMappings'
      { targetMultiMeasureName =
          Prelude.Nothing,
        multiMeasureAttributeMappings =
          Lens.coerced Lens.# pMultiMeasureAttributeMappings_
      }

multiMeasureMappings_targetMultiMeasureName :: Lens.Lens' MultiMeasureMappings (Prelude.Maybe Prelude.Text)
multiMeasureMappings_targetMultiMeasureName = Lens.lens (\MultiMeasureMappings' {targetMultiMeasureName} -> targetMultiMeasureName) (\s@MultiMeasureMappings' {} a -> s {targetMultiMeasureName = a} :: MultiMeasureMappings)

multiMeasureMappings_multiMeasureAttributeMappings :: Lens.Lens' MultiMeasureMappings (Prelude.NonEmpty MultiMeasureAttributeMapping)
multiMeasureMappings_multiMeasureAttributeMappings = Lens.lens (\MultiMeasureMappings' {multiMeasureAttributeMappings} -> multiMeasureAttributeMappings) (\s@MultiMeasureMappings' {} a -> s {multiMeasureAttributeMappings = a} :: MultiMeasureMappings) Prelude.. Lens.coerced

instance Data.FromJSON MultiMeasureMappings where
  parseJSON =
    Data.withObject
      "MultiMeasureMappings"
      ( \x ->
          MultiMeasureMappings'
            Prelude.<$> (x Data..:? "TargetMultiMeasureName")
            Prelude.<*> (x Data..: "MultiMeasureAttributeMappings")
      )

instance Prelude.Hashable MultiMeasureMappings where
  hashWithSalt _salt MultiMeasureMappings' {..} =
    _salt
      `Prelude.hashWithSalt` targetMultiMeasureName
      `Prelude.hashWithSalt` multiMeasureAttributeMappings

instance Prelude.NFData MultiMeasureMappings where
  rnf MultiMeasureMappings' {..} =
    Prelude.rnf targetMultiMeasureName
      `Prelude.seq` Prelude.rnf multiMeasureAttributeMappings

instance Data.ToJSON MultiMeasureMappings where
  toJSON MultiMeasureMappings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetMultiMeasureName" Data..=)
              Prelude.<$> targetMultiMeasureName,
            Prelude.Just
              ( "MultiMeasureAttributeMappings"
                  Data..= multiMeasureAttributeMappings
              )
          ]
      )
