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
-- Module      : Network.AWS.LookoutEquipment.Types.LabelsInputConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LookoutEquipment.Types.LabelsInputConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutEquipment.Types.LabelsS3InputConfiguration
import qualified Network.AWS.Prelude as Prelude

-- | Contains the configuration information for the S3 location being used to
-- hold label data.
--
-- /See:/ 'newLabelsInputConfiguration' smart constructor.
data LabelsInputConfiguration = LabelsInputConfiguration'
  { -- | Contains location information for the S3 location being used for label
    -- data.
    s3InputConfiguration :: LabelsS3InputConfiguration
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
-- 's3InputConfiguration', 'labelsInputConfiguration_s3InputConfiguration' - Contains location information for the S3 location being used for label
-- data.
newLabelsInputConfiguration ::
  -- | 's3InputConfiguration'
  LabelsS3InputConfiguration ->
  LabelsInputConfiguration
newLabelsInputConfiguration pS3InputConfiguration_ =
  LabelsInputConfiguration'
    { s3InputConfiguration =
        pS3InputConfiguration_
    }

-- | Contains location information for the S3 location being used for label
-- data.
labelsInputConfiguration_s3InputConfiguration :: Lens.Lens' LabelsInputConfiguration LabelsS3InputConfiguration
labelsInputConfiguration_s3InputConfiguration = Lens.lens (\LabelsInputConfiguration' {s3InputConfiguration} -> s3InputConfiguration) (\s@LabelsInputConfiguration' {} a -> s {s3InputConfiguration = a} :: LabelsInputConfiguration)

instance Core.FromJSON LabelsInputConfiguration where
  parseJSON =
    Core.withObject
      "LabelsInputConfiguration"
      ( \x ->
          LabelsInputConfiguration'
            Prelude.<$> (x Core..: "S3InputConfiguration")
      )

instance Prelude.Hashable LabelsInputConfiguration

instance Prelude.NFData LabelsInputConfiguration

instance Core.ToJSON LabelsInputConfiguration where
  toJSON LabelsInputConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "S3InputConfiguration"
                  Core..= s3InputConfiguration
              )
          ]
      )
