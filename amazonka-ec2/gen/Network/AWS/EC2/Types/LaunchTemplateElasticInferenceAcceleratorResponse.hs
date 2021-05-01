{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'newLaunchTemplateElasticInferenceAcceleratorResponse' smart constructor.
data LaunchTemplateElasticInferenceAcceleratorResponse = LaunchTemplateElasticInferenceAcceleratorResponse'
  { -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Prelude.Maybe Prelude.Int,
    -- | The type of elastic inference accelerator. The possible values are
    -- eia1.medium, eia1.large, and eia1.xlarge.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateElasticInferenceAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'launchTemplateElasticInferenceAcceleratorResponse_count' - The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- 'type'', 'launchTemplateElasticInferenceAcceleratorResponse_type' - The type of elastic inference accelerator. The possible values are
-- eia1.medium, eia1.large, and eia1.xlarge.
newLaunchTemplateElasticInferenceAcceleratorResponse ::
  LaunchTemplateElasticInferenceAcceleratorResponse
newLaunchTemplateElasticInferenceAcceleratorResponse =
  LaunchTemplateElasticInferenceAcceleratorResponse'
    { count =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
launchTemplateElasticInferenceAcceleratorResponse_count :: Lens.Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Prelude.Maybe Prelude.Int)
launchTemplateElasticInferenceAcceleratorResponse_count = Lens.lens (\LaunchTemplateElasticInferenceAcceleratorResponse' {count} -> count) (\s@LaunchTemplateElasticInferenceAcceleratorResponse' {} a -> s {count = a} :: LaunchTemplateElasticInferenceAcceleratorResponse)

-- | The type of elastic inference accelerator. The possible values are
-- eia1.medium, eia1.large, and eia1.xlarge.
launchTemplateElasticInferenceAcceleratorResponse_type :: Lens.Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Prelude.Maybe Prelude.Text)
launchTemplateElasticInferenceAcceleratorResponse_type = Lens.lens (\LaunchTemplateElasticInferenceAcceleratorResponse' {type'} -> type') (\s@LaunchTemplateElasticInferenceAcceleratorResponse' {} a -> s {type' = a} :: LaunchTemplateElasticInferenceAcceleratorResponse)

instance
  Prelude.FromXML
    LaunchTemplateElasticInferenceAcceleratorResponse
  where
  parseXML x =
    LaunchTemplateElasticInferenceAcceleratorResponse'
      Prelude.<$> (x Prelude..@? "count")
        Prelude.<*> (x Prelude..@? "type")

instance
  Prelude.Hashable
    LaunchTemplateElasticInferenceAcceleratorResponse

instance
  Prelude.NFData
    LaunchTemplateElasticInferenceAcceleratorResponse
