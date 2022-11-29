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
-- Module      : Amazonka.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'newLaunchTemplateElasticInferenceAcceleratorResponse' smart constructor.
data LaunchTemplateElasticInferenceAcceleratorResponse = LaunchTemplateElasticInferenceAcceleratorResponse'
  { -- | The type of elastic inference accelerator. The possible values are
    -- eia1.medium, eia1.large, and eia1.xlarge.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateElasticInferenceAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'launchTemplateElasticInferenceAcceleratorResponse_type' - The type of elastic inference accelerator. The possible values are
-- eia1.medium, eia1.large, and eia1.xlarge.
--
-- 'count', 'launchTemplateElasticInferenceAcceleratorResponse_count' - The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
newLaunchTemplateElasticInferenceAcceleratorResponse ::
  LaunchTemplateElasticInferenceAcceleratorResponse
newLaunchTemplateElasticInferenceAcceleratorResponse =
  LaunchTemplateElasticInferenceAcceleratorResponse'
    { type' =
        Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The type of elastic inference accelerator. The possible values are
-- eia1.medium, eia1.large, and eia1.xlarge.
launchTemplateElasticInferenceAcceleratorResponse_type :: Lens.Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Prelude.Maybe Prelude.Text)
launchTemplateElasticInferenceAcceleratorResponse_type = Lens.lens (\LaunchTemplateElasticInferenceAcceleratorResponse' {type'} -> type') (\s@LaunchTemplateElasticInferenceAcceleratorResponse' {} a -> s {type' = a} :: LaunchTemplateElasticInferenceAcceleratorResponse)

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
launchTemplateElasticInferenceAcceleratorResponse_count :: Lens.Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Prelude.Maybe Prelude.Int)
launchTemplateElasticInferenceAcceleratorResponse_count = Lens.lens (\LaunchTemplateElasticInferenceAcceleratorResponse' {count} -> count) (\s@LaunchTemplateElasticInferenceAcceleratorResponse' {} a -> s {count = a} :: LaunchTemplateElasticInferenceAcceleratorResponse)

instance
  Core.FromXML
    LaunchTemplateElasticInferenceAcceleratorResponse
  where
  parseXML x =
    LaunchTemplateElasticInferenceAcceleratorResponse'
      Prelude.<$> (x Core..@? "type") Prelude.<*> (x Core..@? "count")

instance
  Prelude.Hashable
    LaunchTemplateElasticInferenceAcceleratorResponse
  where
  hashWithSalt
    _salt
    LaunchTemplateElasticInferenceAcceleratorResponse' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` count

instance
  Prelude.NFData
    LaunchTemplateElasticInferenceAcceleratorResponse
  where
  rnf
    LaunchTemplateElasticInferenceAcceleratorResponse' {..} =
      Prelude.rnf type' `Prelude.seq` Prelude.rnf count
