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
-- Module      : Amazonka.DevOpsGuru.Types.AnomalySourceMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalySourceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Metadata about the detection source that generates proactive anomalies.
-- The anomaly is detected using analysis of the metric data  over a period
-- of time
--
-- /See:/ 'newAnomalySourceMetadata' smart constructor.
data AnomalySourceMetadata = AnomalySourceMetadata'
  { -- | The name of the anomaly\'s resource.
    sourceResourceName :: Prelude.Maybe Prelude.Text,
    -- | The anomaly\'s resource type.
    sourceResourceType :: Prelude.Maybe Prelude.Text,
    -- | The source of the anomaly.
    source :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalySourceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceResourceName', 'anomalySourceMetadata_sourceResourceName' - The name of the anomaly\'s resource.
--
-- 'sourceResourceType', 'anomalySourceMetadata_sourceResourceType' - The anomaly\'s resource type.
--
-- 'source', 'anomalySourceMetadata_source' - The source of the anomaly.
newAnomalySourceMetadata ::
  AnomalySourceMetadata
newAnomalySourceMetadata =
  AnomalySourceMetadata'
    { sourceResourceName =
        Prelude.Nothing,
      sourceResourceType = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The name of the anomaly\'s resource.
anomalySourceMetadata_sourceResourceName :: Lens.Lens' AnomalySourceMetadata (Prelude.Maybe Prelude.Text)
anomalySourceMetadata_sourceResourceName = Lens.lens (\AnomalySourceMetadata' {sourceResourceName} -> sourceResourceName) (\s@AnomalySourceMetadata' {} a -> s {sourceResourceName = a} :: AnomalySourceMetadata)

-- | The anomaly\'s resource type.
anomalySourceMetadata_sourceResourceType :: Lens.Lens' AnomalySourceMetadata (Prelude.Maybe Prelude.Text)
anomalySourceMetadata_sourceResourceType = Lens.lens (\AnomalySourceMetadata' {sourceResourceType} -> sourceResourceType) (\s@AnomalySourceMetadata' {} a -> s {sourceResourceType = a} :: AnomalySourceMetadata)

-- | The source of the anomaly.
anomalySourceMetadata_source :: Lens.Lens' AnomalySourceMetadata (Prelude.Maybe Prelude.Text)
anomalySourceMetadata_source = Lens.lens (\AnomalySourceMetadata' {source} -> source) (\s@AnomalySourceMetadata' {} a -> s {source = a} :: AnomalySourceMetadata)

instance Core.FromJSON AnomalySourceMetadata where
  parseJSON =
    Core.withObject
      "AnomalySourceMetadata"
      ( \x ->
          AnomalySourceMetadata'
            Prelude.<$> (x Core..:? "SourceResourceName")
            Prelude.<*> (x Core..:? "SourceResourceType")
            Prelude.<*> (x Core..:? "Source")
      )

instance Prelude.Hashable AnomalySourceMetadata where
  hashWithSalt _salt AnomalySourceMetadata' {..} =
    _salt `Prelude.hashWithSalt` sourceResourceName
      `Prelude.hashWithSalt` sourceResourceType
      `Prelude.hashWithSalt` source

instance Prelude.NFData AnomalySourceMetadata where
  rnf AnomalySourceMetadata' {..} =
    Prelude.rnf sourceResourceName
      `Prelude.seq` Prelude.rnf sourceResourceType
      `Prelude.seq` Prelude.rnf source
