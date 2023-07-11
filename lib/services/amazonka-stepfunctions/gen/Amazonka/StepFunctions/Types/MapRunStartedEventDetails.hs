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
-- Module      : Amazonka.StepFunctions.Types.MapRunStartedEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.MapRunStartedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a Map Run that was started during a state machine
-- execution.
--
-- /See:/ 'newMapRunStartedEventDetails' smart constructor.
data MapRunStartedEventDetails = MapRunStartedEventDetails'
  { -- | The Amazon Resource Name (ARN) of a Map Run that was started.
    mapRunArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapRunStartedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mapRunArn', 'mapRunStartedEventDetails_mapRunArn' - The Amazon Resource Name (ARN) of a Map Run that was started.
newMapRunStartedEventDetails ::
  MapRunStartedEventDetails
newMapRunStartedEventDetails =
  MapRunStartedEventDetails'
    { mapRunArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a Map Run that was started.
mapRunStartedEventDetails_mapRunArn :: Lens.Lens' MapRunStartedEventDetails (Prelude.Maybe Prelude.Text)
mapRunStartedEventDetails_mapRunArn = Lens.lens (\MapRunStartedEventDetails' {mapRunArn} -> mapRunArn) (\s@MapRunStartedEventDetails' {} a -> s {mapRunArn = a} :: MapRunStartedEventDetails)

instance Data.FromJSON MapRunStartedEventDetails where
  parseJSON =
    Data.withObject
      "MapRunStartedEventDetails"
      ( \x ->
          MapRunStartedEventDetails'
            Prelude.<$> (x Data..:? "mapRunArn")
      )

instance Prelude.Hashable MapRunStartedEventDetails where
  hashWithSalt _salt MapRunStartedEventDetails' {..} =
    _salt `Prelude.hashWithSalt` mapRunArn

instance Prelude.NFData MapRunStartedEventDetails where
  rnf MapRunStartedEventDetails' {..} =
    Prelude.rnf mapRunArn
