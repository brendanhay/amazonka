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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details for an Amazon Elastic Inference accelerator.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' smart constructor.
data AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails = AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails'
  { -- | The number of Elastic Inference accelerators to attach to the instance.
    count :: Prelude.Maybe Prelude.Int,
    -- | The type of Elastic Inference accelerator.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_count' - The number of Elastic Inference accelerators to attach to the instance.
--
-- 'type'', 'awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_type' - The type of Elastic Inference accelerator.
newAwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails ::
  AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
newAwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails =
  AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails'
    { count =
        Prelude.Nothing,
      type' =
        Prelude.Nothing
    }

-- | The number of Elastic Inference accelerators to attach to the instance.
awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_count :: Lens.Lens' AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_count = Lens.lens (\AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' {count} -> count) (\s@AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' {} a -> s {count = a} :: AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails)

-- | The type of Elastic Inference accelerator.
awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_type :: Lens.Lens' AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_type = Lens.lens (\AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' {type'} -> type') (\s@AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' {} a -> s {type' = a} :: AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails'
            Prelude.<$> (x Data..:? "Count")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' {..} =
      _salt
        `Prelude.hashWithSalt` count
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
  where
  rnf
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' {..} =
      Prelude.rnf count `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Count" Data..=) Prelude.<$> count,
              ("Type" Data..=) Prelude.<$> type'
            ]
        )
