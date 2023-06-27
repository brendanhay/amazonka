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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.OutputResolutionStackInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.OutputResolutionStackInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.PredefinedResolution
import Amazonka.SageMakerGeoSpatial.Types.UserDefined

-- | The input structure representing Output Resolution for Stacking
-- Operation.
--
-- /See:/ 'newOutputResolutionStackInput' smart constructor.
data OutputResolutionStackInput = OutputResolutionStackInput'
  { -- | A string value representing Predefined Output Resolution for a stacking
    -- operation. Allowed values are @HIGHEST@, @LOWEST@, and @AVERAGE@.
    predefined :: Prelude.Maybe PredefinedResolution,
    -- | The structure representing User Output Resolution for a Stacking
    -- operation defined as a value and unit.
    userDefined :: Prelude.Maybe UserDefined
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputResolutionStackInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predefined', 'outputResolutionStackInput_predefined' - A string value representing Predefined Output Resolution for a stacking
-- operation. Allowed values are @HIGHEST@, @LOWEST@, and @AVERAGE@.
--
-- 'userDefined', 'outputResolutionStackInput_userDefined' - The structure representing User Output Resolution for a Stacking
-- operation defined as a value and unit.
newOutputResolutionStackInput ::
  OutputResolutionStackInput
newOutputResolutionStackInput =
  OutputResolutionStackInput'
    { predefined =
        Prelude.Nothing,
      userDefined = Prelude.Nothing
    }

-- | A string value representing Predefined Output Resolution for a stacking
-- operation. Allowed values are @HIGHEST@, @LOWEST@, and @AVERAGE@.
outputResolutionStackInput_predefined :: Lens.Lens' OutputResolutionStackInput (Prelude.Maybe PredefinedResolution)
outputResolutionStackInput_predefined = Lens.lens (\OutputResolutionStackInput' {predefined} -> predefined) (\s@OutputResolutionStackInput' {} a -> s {predefined = a} :: OutputResolutionStackInput)

-- | The structure representing User Output Resolution for a Stacking
-- operation defined as a value and unit.
outputResolutionStackInput_userDefined :: Lens.Lens' OutputResolutionStackInput (Prelude.Maybe UserDefined)
outputResolutionStackInput_userDefined = Lens.lens (\OutputResolutionStackInput' {userDefined} -> userDefined) (\s@OutputResolutionStackInput' {} a -> s {userDefined = a} :: OutputResolutionStackInput)

instance Data.FromJSON OutputResolutionStackInput where
  parseJSON =
    Data.withObject
      "OutputResolutionStackInput"
      ( \x ->
          OutputResolutionStackInput'
            Prelude.<$> (x Data..:? "Predefined")
            Prelude.<*> (x Data..:? "UserDefined")
      )

instance Prelude.Hashable OutputResolutionStackInput where
  hashWithSalt _salt OutputResolutionStackInput' {..} =
    _salt
      `Prelude.hashWithSalt` predefined
      `Prelude.hashWithSalt` userDefined

instance Prelude.NFData OutputResolutionStackInput where
  rnf OutputResolutionStackInput' {..} =
    Prelude.rnf predefined
      `Prelude.seq` Prelude.rnf userDefined

instance Data.ToJSON OutputResolutionStackInput where
  toJSON OutputResolutionStackInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Predefined" Data..=) Prelude.<$> predefined,
            ("UserDefined" Data..=) Prelude.<$> userDefined
          ]
      )
