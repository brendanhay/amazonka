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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about an Elastic Graphics specification for an Amazon
-- EC2 launch template.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails' smart constructor.
data AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails = AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails'
  { -- | The type of Elastic Graphics accelerator.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails_type' - The type of Elastic Graphics accelerator.
newAwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails ::
  AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
newAwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails =
  AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails'
    { type' =
        Prelude.Nothing
    }

-- | The type of Elastic Graphics accelerator.
awsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails_type :: Lens.Lens' AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails_type = Lens.lens (\AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails' {type'} -> type') (\s@AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails' {} a -> s {type' = a} :: AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails'
            Prelude.<$> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails' {..} =
      _salt `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
  where
  rnf
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails' {..} =
      Prelude.rnf type'

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Type" Data..=) Prelude.<$> type']
        )
