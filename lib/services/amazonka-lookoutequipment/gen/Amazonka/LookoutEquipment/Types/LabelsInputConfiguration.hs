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
-- Module      : Amazonka.LookoutEquipment.Types.LabelsInputConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.LabelsInputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.LabelsS3InputConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information for the S3 location being used to
-- hold label data.
--
-- /See:/ 'newLabelsInputConfiguration' smart constructor.
data LabelsInputConfiguration = LabelsInputConfiguration'
  { -- | The name of the label group to be used for label data.
    labelGroupName :: Prelude.Maybe Prelude.Text,
    -- | Contains location information for the S3 location being used for label
    -- data.
    s3InputConfiguration :: Prelude.Maybe LabelsS3InputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelsInputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupName', 'labelsInputConfiguration_labelGroupName' - The name of the label group to be used for label data.
--
-- 's3InputConfiguration', 'labelsInputConfiguration_s3InputConfiguration' - Contains location information for the S3 location being used for label
-- data.
newLabelsInputConfiguration ::
  LabelsInputConfiguration
newLabelsInputConfiguration =
  LabelsInputConfiguration'
    { labelGroupName =
        Prelude.Nothing,
      s3InputConfiguration = Prelude.Nothing
    }

-- | The name of the label group to be used for label data.
labelsInputConfiguration_labelGroupName :: Lens.Lens' LabelsInputConfiguration (Prelude.Maybe Prelude.Text)
labelsInputConfiguration_labelGroupName = Lens.lens (\LabelsInputConfiguration' {labelGroupName} -> labelGroupName) (\s@LabelsInputConfiguration' {} a -> s {labelGroupName = a} :: LabelsInputConfiguration)

-- | Contains location information for the S3 location being used for label
-- data.
labelsInputConfiguration_s3InputConfiguration :: Lens.Lens' LabelsInputConfiguration (Prelude.Maybe LabelsS3InputConfiguration)
labelsInputConfiguration_s3InputConfiguration = Lens.lens (\LabelsInputConfiguration' {s3InputConfiguration} -> s3InputConfiguration) (\s@LabelsInputConfiguration' {} a -> s {s3InputConfiguration = a} :: LabelsInputConfiguration)

instance Data.FromJSON LabelsInputConfiguration where
  parseJSON =
    Data.withObject
      "LabelsInputConfiguration"
      ( \x ->
          LabelsInputConfiguration'
            Prelude.<$> (x Data..:? "LabelGroupName")
            Prelude.<*> (x Data..:? "S3InputConfiguration")
      )

instance Prelude.Hashable LabelsInputConfiguration where
  hashWithSalt _salt LabelsInputConfiguration' {..} =
    _salt `Prelude.hashWithSalt` labelGroupName
      `Prelude.hashWithSalt` s3InputConfiguration

instance Prelude.NFData LabelsInputConfiguration where
  rnf LabelsInputConfiguration' {..} =
    Prelude.rnf labelGroupName
      `Prelude.seq` Prelude.rnf s3InputConfiguration

instance Data.ToJSON LabelsInputConfiguration where
  toJSON LabelsInputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LabelGroupName" Data..=)
              Prelude.<$> labelGroupName,
            ("S3InputConfiguration" Data..=)
              Prelude.<$> s3InputConfiguration
          ]
      )
