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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of network interfaces to be attached to
-- an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails = AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails'
  { -- | The maximum number of network interfaces.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of network interfaces.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_max' - The maximum number of network interfaces.
--
-- 'min', 'awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_min' - The minimum number of network interfaces.
newAwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails'
    { max =
        Prelude.Nothing,
      min =
        Prelude.Nothing
    }

-- | The maximum number of network interfaces.
awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_max :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_max = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' {max} -> max) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' {} a -> s {max = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails)

-- | The minimum number of network interfaces.
awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_min :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_min = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' {min} -> min) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' {} a -> s {min = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails'
            Prelude.<$> (x Data..:? "Max") Prelude.<*> (x Data..:? "Min")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' {..} =
      Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Max" Data..=) Prelude.<$> max,
              ("Min" Data..=) Prelude.<$> min
            ]
        )
