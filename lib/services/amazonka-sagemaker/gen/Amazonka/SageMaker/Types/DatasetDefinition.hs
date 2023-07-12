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
-- Module      : Amazonka.SageMaker.Types.DatasetDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DatasetDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AthenaDatasetDefinition
import Amazonka.SageMaker.Types.DataDistributionType
import Amazonka.SageMaker.Types.InputMode
import Amazonka.SageMaker.Types.RedshiftDatasetDefinition

-- | Configuration for Dataset Definition inputs. The Dataset Definition
-- input must specify exactly one of either @AthenaDatasetDefinition@ or
-- @RedshiftDatasetDefinition@ types.
--
-- /See:/ 'newDatasetDefinition' smart constructor.
data DatasetDefinition = DatasetDefinition'
  { athenaDatasetDefinition :: Prelude.Maybe AthenaDatasetDefinition,
    -- | Whether the generated dataset is @FullyReplicated@ or @ShardedByS3Key@
    -- (default).
    dataDistributionType :: Prelude.Maybe DataDistributionType,
    -- | Whether to use @File@ or @Pipe@ input mode. In @File@ (default) mode,
    -- Amazon SageMaker copies the data from the input source onto the local
    -- Amazon Elastic Block Store (Amazon EBS) volumes before starting your
    -- training algorithm. This is the most commonly used input mode. In @Pipe@
    -- mode, Amazon SageMaker streams input data from the source directly to
    -- your algorithm without using the EBS volume.
    inputMode :: Prelude.Maybe InputMode,
    -- | The local path where you want Amazon SageMaker to download the Dataset
    -- Definition inputs to run a processing job. @LocalPath@ is an absolute
    -- path to the input data. This is a required parameter when @AppManaged@
    -- is @False@ (default).
    localPath :: Prelude.Maybe Prelude.Text,
    redshiftDatasetDefinition :: Prelude.Maybe RedshiftDatasetDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'athenaDatasetDefinition', 'datasetDefinition_athenaDatasetDefinition' - Undocumented member.
--
-- 'dataDistributionType', 'datasetDefinition_dataDistributionType' - Whether the generated dataset is @FullyReplicated@ or @ShardedByS3Key@
-- (default).
--
-- 'inputMode', 'datasetDefinition_inputMode' - Whether to use @File@ or @Pipe@ input mode. In @File@ (default) mode,
-- Amazon SageMaker copies the data from the input source onto the local
-- Amazon Elastic Block Store (Amazon EBS) volumes before starting your
-- training algorithm. This is the most commonly used input mode. In @Pipe@
-- mode, Amazon SageMaker streams input data from the source directly to
-- your algorithm without using the EBS volume.
--
-- 'localPath', 'datasetDefinition_localPath' - The local path where you want Amazon SageMaker to download the Dataset
-- Definition inputs to run a processing job. @LocalPath@ is an absolute
-- path to the input data. This is a required parameter when @AppManaged@
-- is @False@ (default).
--
-- 'redshiftDatasetDefinition', 'datasetDefinition_redshiftDatasetDefinition' - Undocumented member.
newDatasetDefinition ::
  DatasetDefinition
newDatasetDefinition =
  DatasetDefinition'
    { athenaDatasetDefinition =
        Prelude.Nothing,
      dataDistributionType = Prelude.Nothing,
      inputMode = Prelude.Nothing,
      localPath = Prelude.Nothing,
      redshiftDatasetDefinition = Prelude.Nothing
    }

-- | Undocumented member.
datasetDefinition_athenaDatasetDefinition :: Lens.Lens' DatasetDefinition (Prelude.Maybe AthenaDatasetDefinition)
datasetDefinition_athenaDatasetDefinition = Lens.lens (\DatasetDefinition' {athenaDatasetDefinition} -> athenaDatasetDefinition) (\s@DatasetDefinition' {} a -> s {athenaDatasetDefinition = a} :: DatasetDefinition)

-- | Whether the generated dataset is @FullyReplicated@ or @ShardedByS3Key@
-- (default).
datasetDefinition_dataDistributionType :: Lens.Lens' DatasetDefinition (Prelude.Maybe DataDistributionType)
datasetDefinition_dataDistributionType = Lens.lens (\DatasetDefinition' {dataDistributionType} -> dataDistributionType) (\s@DatasetDefinition' {} a -> s {dataDistributionType = a} :: DatasetDefinition)

-- | Whether to use @File@ or @Pipe@ input mode. In @File@ (default) mode,
-- Amazon SageMaker copies the data from the input source onto the local
-- Amazon Elastic Block Store (Amazon EBS) volumes before starting your
-- training algorithm. This is the most commonly used input mode. In @Pipe@
-- mode, Amazon SageMaker streams input data from the source directly to
-- your algorithm without using the EBS volume.
datasetDefinition_inputMode :: Lens.Lens' DatasetDefinition (Prelude.Maybe InputMode)
datasetDefinition_inputMode = Lens.lens (\DatasetDefinition' {inputMode} -> inputMode) (\s@DatasetDefinition' {} a -> s {inputMode = a} :: DatasetDefinition)

-- | The local path where you want Amazon SageMaker to download the Dataset
-- Definition inputs to run a processing job. @LocalPath@ is an absolute
-- path to the input data. This is a required parameter when @AppManaged@
-- is @False@ (default).
datasetDefinition_localPath :: Lens.Lens' DatasetDefinition (Prelude.Maybe Prelude.Text)
datasetDefinition_localPath = Lens.lens (\DatasetDefinition' {localPath} -> localPath) (\s@DatasetDefinition' {} a -> s {localPath = a} :: DatasetDefinition)

-- | Undocumented member.
datasetDefinition_redshiftDatasetDefinition :: Lens.Lens' DatasetDefinition (Prelude.Maybe RedshiftDatasetDefinition)
datasetDefinition_redshiftDatasetDefinition = Lens.lens (\DatasetDefinition' {redshiftDatasetDefinition} -> redshiftDatasetDefinition) (\s@DatasetDefinition' {} a -> s {redshiftDatasetDefinition = a} :: DatasetDefinition)

instance Data.FromJSON DatasetDefinition where
  parseJSON =
    Data.withObject
      "DatasetDefinition"
      ( \x ->
          DatasetDefinition'
            Prelude.<$> (x Data..:? "AthenaDatasetDefinition")
            Prelude.<*> (x Data..:? "DataDistributionType")
            Prelude.<*> (x Data..:? "InputMode")
            Prelude.<*> (x Data..:? "LocalPath")
            Prelude.<*> (x Data..:? "RedshiftDatasetDefinition")
      )

instance Prelude.Hashable DatasetDefinition where
  hashWithSalt _salt DatasetDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` athenaDatasetDefinition
      `Prelude.hashWithSalt` dataDistributionType
      `Prelude.hashWithSalt` inputMode
      `Prelude.hashWithSalt` localPath
      `Prelude.hashWithSalt` redshiftDatasetDefinition

instance Prelude.NFData DatasetDefinition where
  rnf DatasetDefinition' {..} =
    Prelude.rnf athenaDatasetDefinition
      `Prelude.seq` Prelude.rnf dataDistributionType
      `Prelude.seq` Prelude.rnf inputMode
      `Prelude.seq` Prelude.rnf localPath
      `Prelude.seq` Prelude.rnf redshiftDatasetDefinition

instance Data.ToJSON DatasetDefinition where
  toJSON DatasetDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AthenaDatasetDefinition" Data..=)
              Prelude.<$> athenaDatasetDefinition,
            ("DataDistributionType" Data..=)
              Prelude.<$> dataDistributionType,
            ("InputMode" Data..=) Prelude.<$> inputMode,
            ("LocalPath" Data..=) Prelude.<$> localPath,
            ("RedshiftDatasetDefinition" Data..=)
              Prelude.<$> redshiftDatasetDefinition
          ]
      )
