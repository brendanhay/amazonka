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
-- Module      : Network.AWS.StepFunctions.Types.MapIterationEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.MapIterationEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about an iteration of a Map state.
--
-- /See:/ 'newMapIterationEventDetails' smart constructor.
data MapIterationEventDetails = MapIterationEventDetails'
  { -- | The name of the iteration’s parent Map state.
    name :: Prelude.Maybe Prelude.Text,
    -- | The index of the array belonging to the Map state iteration.
    index :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapIterationEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'mapIterationEventDetails_name' - The name of the iteration’s parent Map state.
--
-- 'index', 'mapIterationEventDetails_index' - The index of the array belonging to the Map state iteration.
newMapIterationEventDetails ::
  MapIterationEventDetails
newMapIterationEventDetails =
  MapIterationEventDetails'
    { name = Prelude.Nothing,
      index = Prelude.Nothing
    }

-- | The name of the iteration’s parent Map state.
mapIterationEventDetails_name :: Lens.Lens' MapIterationEventDetails (Prelude.Maybe Prelude.Text)
mapIterationEventDetails_name = Lens.lens (\MapIterationEventDetails' {name} -> name) (\s@MapIterationEventDetails' {} a -> s {name = a} :: MapIterationEventDetails)

-- | The index of the array belonging to the Map state iteration.
mapIterationEventDetails_index :: Lens.Lens' MapIterationEventDetails (Prelude.Maybe Prelude.Natural)
mapIterationEventDetails_index = Lens.lens (\MapIterationEventDetails' {index} -> index) (\s@MapIterationEventDetails' {} a -> s {index = a} :: MapIterationEventDetails)

instance Core.FromJSON MapIterationEventDetails where
  parseJSON =
    Core.withObject
      "MapIterationEventDetails"
      ( \x ->
          MapIterationEventDetails'
            Prelude.<$> (x Core..:? "name") Prelude.<*> (x Core..:? "index")
      )

instance Prelude.Hashable MapIterationEventDetails

instance Prelude.NFData MapIterationEventDetails
