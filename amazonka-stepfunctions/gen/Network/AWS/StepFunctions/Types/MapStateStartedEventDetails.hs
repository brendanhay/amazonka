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
-- Module      : Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.MapStateStartedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about a Map state that was started.
--
-- /See:/ 'newMapStateStartedEventDetails' smart constructor.
data MapStateStartedEventDetails = MapStateStartedEventDetails'
  { -- | The size of the array for Map state iterations.
    length :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MapStateStartedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'length', 'mapStateStartedEventDetails_length' - The size of the array for Map state iterations.
newMapStateStartedEventDetails ::
  MapStateStartedEventDetails
newMapStateStartedEventDetails =
  MapStateStartedEventDetails'
    { length =
        Prelude.Nothing
    }

-- | The size of the array for Map state iterations.
mapStateStartedEventDetails_length :: Lens.Lens' MapStateStartedEventDetails (Prelude.Maybe Prelude.Natural)
mapStateStartedEventDetails_length = Lens.lens (\MapStateStartedEventDetails' {length} -> length) (\s@MapStateStartedEventDetails' {} a -> s {length = a} :: MapStateStartedEventDetails)

instance Prelude.FromJSON MapStateStartedEventDetails where
  parseJSON =
    Prelude.withObject
      "MapStateStartedEventDetails"
      ( \x ->
          MapStateStartedEventDetails'
            Prelude.<$> (x Prelude..:? "length")
      )

instance Prelude.Hashable MapStateStartedEventDetails

instance Prelude.NFData MapStateStartedEventDetails
