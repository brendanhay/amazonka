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
-- Module      : Amazonka.Panorama.Types.OTAJobConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.OTAJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An over-the-air update (OTA) job configuration.
--
-- /See:/ 'newOTAJobConfig' smart constructor.
data OTAJobConfig = OTAJobConfig'
  { -- | The target version of the device software.
    imageVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OTAJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageVersion', 'oTAJobConfig_imageVersion' - The target version of the device software.
newOTAJobConfig ::
  -- | 'imageVersion'
  Prelude.Text ->
  OTAJobConfig
newOTAJobConfig pImageVersion_ =
  OTAJobConfig' {imageVersion = pImageVersion_}

-- | The target version of the device software.
oTAJobConfig_imageVersion :: Lens.Lens' OTAJobConfig Prelude.Text
oTAJobConfig_imageVersion = Lens.lens (\OTAJobConfig' {imageVersion} -> imageVersion) (\s@OTAJobConfig' {} a -> s {imageVersion = a} :: OTAJobConfig)

instance Prelude.Hashable OTAJobConfig where
  hashWithSalt _salt OTAJobConfig' {..} =
    _salt `Prelude.hashWithSalt` imageVersion

instance Prelude.NFData OTAJobConfig where
  rnf OTAJobConfig' {..} = Prelude.rnf imageVersion

instance Data.ToJSON OTAJobConfig where
  toJSON OTAJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImageVersion" Data..= imageVersion)]
      )
