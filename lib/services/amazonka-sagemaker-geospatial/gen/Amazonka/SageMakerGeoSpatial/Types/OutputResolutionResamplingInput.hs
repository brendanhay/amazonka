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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.OutputResolutionResamplingInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.OutputResolutionResamplingInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.UserDefined

-- |
--
-- /See:/ 'newOutputResolutionResamplingInput' smart constructor.
data OutputResolutionResamplingInput = OutputResolutionResamplingInput'
  { userDefined :: UserDefined
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputResolutionResamplingInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userDefined', 'outputResolutionResamplingInput_userDefined' -
newOutputResolutionResamplingInput ::
  -- | 'userDefined'
  UserDefined ->
  OutputResolutionResamplingInput
newOutputResolutionResamplingInput pUserDefined_ =
  OutputResolutionResamplingInput'
    { userDefined =
        pUserDefined_
    }

-- |
outputResolutionResamplingInput_userDefined :: Lens.Lens' OutputResolutionResamplingInput UserDefined
outputResolutionResamplingInput_userDefined = Lens.lens (\OutputResolutionResamplingInput' {userDefined} -> userDefined) (\s@OutputResolutionResamplingInput' {} a -> s {userDefined = a} :: OutputResolutionResamplingInput)

instance
  Data.FromJSON
    OutputResolutionResamplingInput
  where
  parseJSON =
    Data.withObject
      "OutputResolutionResamplingInput"
      ( \x ->
          OutputResolutionResamplingInput'
            Prelude.<$> (x Data..: "UserDefined")
      )

instance
  Prelude.Hashable
    OutputResolutionResamplingInput
  where
  hashWithSalt
    _salt
    OutputResolutionResamplingInput' {..} =
      _salt `Prelude.hashWithSalt` userDefined

instance
  Prelude.NFData
    OutputResolutionResamplingInput
  where
  rnf OutputResolutionResamplingInput' {..} =
    Prelude.rnf userDefined

instance Data.ToJSON OutputResolutionResamplingInput where
  toJSON OutputResolutionResamplingInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserDefined" Data..= userDefined)]
      )
