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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceInputConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceInputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.InferenceInputNameConfiguration
import Amazonka.LookoutEquipment.Types.InferenceS3InputConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration information for the input data for the
-- inference, including Amazon S3 location of input data..
--
-- /See:/ 'newInferenceInputConfiguration' smart constructor.
data InferenceInputConfiguration = InferenceInputConfiguration'
  { -- | Specifies configuration information for the input data for the
    -- inference, including timestamp format and delimiter.
    inferenceInputNameConfiguration :: Prelude.Maybe InferenceInputNameConfiguration,
    -- | Indicates the difference between your time zone and Coordinated
    -- Universal Time (UTC).
    inputTimeZoneOffset :: Prelude.Maybe Prelude.Text,
    -- | Specifies configuration information for the input data for the
    -- inference, including Amazon S3 location of input data.
    s3InputConfiguration :: Prelude.Maybe InferenceS3InputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceInputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceInputNameConfiguration', 'inferenceInputConfiguration_inferenceInputNameConfiguration' - Specifies configuration information for the input data for the
-- inference, including timestamp format and delimiter.
--
-- 'inputTimeZoneOffset', 'inferenceInputConfiguration_inputTimeZoneOffset' - Indicates the difference between your time zone and Coordinated
-- Universal Time (UTC).
--
-- 's3InputConfiguration', 'inferenceInputConfiguration_s3InputConfiguration' - Specifies configuration information for the input data for the
-- inference, including Amazon S3 location of input data.
newInferenceInputConfiguration ::
  InferenceInputConfiguration
newInferenceInputConfiguration =
  InferenceInputConfiguration'
    { inferenceInputNameConfiguration =
        Prelude.Nothing,
      inputTimeZoneOffset = Prelude.Nothing,
      s3InputConfiguration = Prelude.Nothing
    }

-- | Specifies configuration information for the input data for the
-- inference, including timestamp format and delimiter.
inferenceInputConfiguration_inferenceInputNameConfiguration :: Lens.Lens' InferenceInputConfiguration (Prelude.Maybe InferenceInputNameConfiguration)
inferenceInputConfiguration_inferenceInputNameConfiguration = Lens.lens (\InferenceInputConfiguration' {inferenceInputNameConfiguration} -> inferenceInputNameConfiguration) (\s@InferenceInputConfiguration' {} a -> s {inferenceInputNameConfiguration = a} :: InferenceInputConfiguration)

-- | Indicates the difference between your time zone and Coordinated
-- Universal Time (UTC).
inferenceInputConfiguration_inputTimeZoneOffset :: Lens.Lens' InferenceInputConfiguration (Prelude.Maybe Prelude.Text)
inferenceInputConfiguration_inputTimeZoneOffset = Lens.lens (\InferenceInputConfiguration' {inputTimeZoneOffset} -> inputTimeZoneOffset) (\s@InferenceInputConfiguration' {} a -> s {inputTimeZoneOffset = a} :: InferenceInputConfiguration)

-- | Specifies configuration information for the input data for the
-- inference, including Amazon S3 location of input data.
inferenceInputConfiguration_s3InputConfiguration :: Lens.Lens' InferenceInputConfiguration (Prelude.Maybe InferenceS3InputConfiguration)
inferenceInputConfiguration_s3InputConfiguration = Lens.lens (\InferenceInputConfiguration' {s3InputConfiguration} -> s3InputConfiguration) (\s@InferenceInputConfiguration' {} a -> s {s3InputConfiguration = a} :: InferenceInputConfiguration)

instance Data.FromJSON InferenceInputConfiguration where
  parseJSON =
    Data.withObject
      "InferenceInputConfiguration"
      ( \x ->
          InferenceInputConfiguration'
            Prelude.<$> (x Data..:? "InferenceInputNameConfiguration")
            Prelude.<*> (x Data..:? "InputTimeZoneOffset")
            Prelude.<*> (x Data..:? "S3InputConfiguration")
      )

instance Prelude.Hashable InferenceInputConfiguration where
  hashWithSalt _salt InferenceInputConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` inferenceInputNameConfiguration
      `Prelude.hashWithSalt` inputTimeZoneOffset
      `Prelude.hashWithSalt` s3InputConfiguration

instance Prelude.NFData InferenceInputConfiguration where
  rnf InferenceInputConfiguration' {..} =
    Prelude.rnf inferenceInputNameConfiguration
      `Prelude.seq` Prelude.rnf inputTimeZoneOffset
      `Prelude.seq` Prelude.rnf s3InputConfiguration

instance Data.ToJSON InferenceInputConfiguration where
  toJSON InferenceInputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InferenceInputNameConfiguration" Data..=)
              Prelude.<$> inferenceInputNameConfiguration,
            ("InputTimeZoneOffset" Data..=)
              Prelude.<$> inputTimeZoneOffset,
            ("S3InputConfiguration" Data..=)
              Prelude.<$> s3InputConfiguration
          ]
      )
