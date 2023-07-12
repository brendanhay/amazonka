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
-- Module      : Amazonka.LookoutVision.Types.Anomaly
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.Anomaly where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.PixelAnomaly
import qualified Amazonka.Prelude as Prelude

-- | Information about an anomaly type found on an image by an image
-- segmentation model. For more information, see DetectAnomalies.
--
-- /See:/ 'newAnomaly' smart constructor.
data Anomaly = Anomaly'
  { -- | The name of an anomaly type found in an image. @Name@ maps to an anomaly
    -- type in the training dataset, apart from the anomaly type @background@.
    -- The service automatically inserts the @background@ anomaly type into the
    -- response from @DetectAnomalies@.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the pixel mask that covers an anomaly type.
    pixelAnomaly :: Prelude.Maybe PixelAnomaly
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Anomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'anomaly_name' - The name of an anomaly type found in an image. @Name@ maps to an anomaly
-- type in the training dataset, apart from the anomaly type @background@.
-- The service automatically inserts the @background@ anomaly type into the
-- response from @DetectAnomalies@.
--
-- 'pixelAnomaly', 'anomaly_pixelAnomaly' - Information about the pixel mask that covers an anomaly type.
newAnomaly ::
  Anomaly
newAnomaly =
  Anomaly'
    { name = Prelude.Nothing,
      pixelAnomaly = Prelude.Nothing
    }

-- | The name of an anomaly type found in an image. @Name@ maps to an anomaly
-- type in the training dataset, apart from the anomaly type @background@.
-- The service automatically inserts the @background@ anomaly type into the
-- response from @DetectAnomalies@.
anomaly_name :: Lens.Lens' Anomaly (Prelude.Maybe Prelude.Text)
anomaly_name = Lens.lens (\Anomaly' {name} -> name) (\s@Anomaly' {} a -> s {name = a} :: Anomaly)

-- | Information about the pixel mask that covers an anomaly type.
anomaly_pixelAnomaly :: Lens.Lens' Anomaly (Prelude.Maybe PixelAnomaly)
anomaly_pixelAnomaly = Lens.lens (\Anomaly' {pixelAnomaly} -> pixelAnomaly) (\s@Anomaly' {} a -> s {pixelAnomaly = a} :: Anomaly)

instance Data.FromJSON Anomaly where
  parseJSON =
    Data.withObject
      "Anomaly"
      ( \x ->
          Anomaly'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PixelAnomaly")
      )

instance Prelude.Hashable Anomaly where
  hashWithSalt _salt Anomaly' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pixelAnomaly

instance Prelude.NFData Anomaly where
  rnf Anomaly' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf pixelAnomaly
