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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalySourceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata about the detection source that generates proactive anomalies.
-- The anomaly is detected using analysis of the metric data  over a period
-- of time
--
-- /See:/ 'newAnomalySourceMetadata' smart constructor.
data AnomalySourceMetadata = AnomalySourceMetadata'
  { -- | The source of the anomaly.
    source :: Prelude.Maybe Prelude.Text,
    -- | The name of the anomaly\'s resource.
    sourceResourceName :: Prelude.Maybe Prelude.Text,
    -- | The anomaly\'s resource type.
    sourceResourceType :: Prelude.Maybe Prelude.Text
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
-- 'source', 'anomalySourceMetadata_source' - The source of the anomaly.
--
-- 'sourceResourceName', 'anomalySourceMetadata_sourceResourceName' - The name of the anomaly\'s resource.
--
-- 'sourceResourceType', 'anomalySourceMetadata_sourceResourceType' - The anomaly\'s resource type.
newAnomalySourceMetadata ::
  AnomalySourceMetadata
newAnomalySourceMetadata =
  AnomalySourceMetadata'
    { source = Prelude.Nothing,
      sourceResourceName = Prelude.Nothing,
      sourceResourceType = Prelude.Nothing
    }

-- | The source of the anomaly.
anomalySourceMetadata_source :: Lens.Lens' AnomalySourceMetadata (Prelude.Maybe Prelude.Text)
anomalySourceMetadata_source = Lens.lens (\AnomalySourceMetadata' {source} -> source) (\s@AnomalySourceMetadata' {} a -> s {source = a} :: AnomalySourceMetadata)

-- | The name of the anomaly\'s resource.
anomalySourceMetadata_sourceResourceName :: Lens.Lens' AnomalySourceMetadata (Prelude.Maybe Prelude.Text)
anomalySourceMetadata_sourceResourceName = Lens.lens (\AnomalySourceMetadata' {sourceResourceName} -> sourceResourceName) (\s@AnomalySourceMetadata' {} a -> s {sourceResourceName = a} :: AnomalySourceMetadata)

-- | The anomaly\'s resource type.
anomalySourceMetadata_sourceResourceType :: Lens.Lens' AnomalySourceMetadata (Prelude.Maybe Prelude.Text)
anomalySourceMetadata_sourceResourceType = Lens.lens (\AnomalySourceMetadata' {sourceResourceType} -> sourceResourceType) (\s@AnomalySourceMetadata' {} a -> s {sourceResourceType = a} :: AnomalySourceMetadata)

instance Data.FromJSON AnomalySourceMetadata where
  parseJSON =
    Data.withObject
      "AnomalySourceMetadata"
      ( \x ->
          AnomalySourceMetadata'
            Prelude.<$> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "SourceResourceName")
            Prelude.<*> (x Data..:? "SourceResourceType")
      )

instance Prelude.Hashable AnomalySourceMetadata where
  hashWithSalt _salt AnomalySourceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` sourceResourceName
      `Prelude.hashWithSalt` sourceResourceType

instance Prelude.NFData AnomalySourceMetadata where
  rnf AnomalySourceMetadata' {..} =
    Prelude.rnf source `Prelude.seq`
      Prelude.rnf sourceResourceName `Prelude.seq`
        Prelude.rnf sourceResourceType
