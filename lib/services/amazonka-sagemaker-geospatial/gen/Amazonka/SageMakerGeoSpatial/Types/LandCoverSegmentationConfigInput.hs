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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.LandCoverSegmentationConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.LandCoverSegmentationConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input structure for Land Cover Operation type.
--
-- /See:/ 'newLandCoverSegmentationConfigInput' smart constructor.
data LandCoverSegmentationConfigInput = LandCoverSegmentationConfigInput'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LandCoverSegmentationConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newLandCoverSegmentationConfigInput ::
  LandCoverSegmentationConfigInput
newLandCoverSegmentationConfigInput =
  LandCoverSegmentationConfigInput'

instance
  Data.FromJSON
    LandCoverSegmentationConfigInput
  where
  parseJSON =
    Data.withObject
      "LandCoverSegmentationConfigInput"
      ( \x ->
          Prelude.pure LandCoverSegmentationConfigInput'
      )

instance
  Prelude.Hashable
    LandCoverSegmentationConfigInput
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    LandCoverSegmentationConfigInput
  where
  rnf _ = ()

instance Data.ToJSON LandCoverSegmentationConfigInput where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
