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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.OutputResolutionStackInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.PredefinedResolution
import Amazonka.SageMakerGeoSpatial.Types.UserDefined

-- |
--
-- /See:/ 'newOutputResolutionStackInput' smart constructor.
data OutputResolutionStackInput = OutputResolutionStackInput'
  { predefined :: Prelude.Maybe PredefinedResolution,
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
-- 'predefined', 'outputResolutionStackInput_predefined' -
--
-- 'userDefined', 'outputResolutionStackInput_userDefined' -
newOutputResolutionStackInput ::
  OutputResolutionStackInput
newOutputResolutionStackInput =
  OutputResolutionStackInput'
    { predefined =
        Prelude.Nothing,
      userDefined = Prelude.Nothing
    }

outputResolutionStackInput_predefined :: Lens.Lens' OutputResolutionStackInput (Prelude.Maybe PredefinedResolution)
outputResolutionStackInput_predefined = Lens.lens (\OutputResolutionStackInput' {predefined} -> predefined) (\s@OutputResolutionStackInput' {} a -> s {predefined = a} :: OutputResolutionStackInput)

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
