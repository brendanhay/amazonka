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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'newLaunchTemplateElasticInferenceAccelerator' smart constructor.
data LaunchTemplateElasticInferenceAccelerator = LaunchTemplateElasticInferenceAccelerator'
  { -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Prelude.Maybe Prelude.Natural,
    -- | The type of elastic inference accelerator. The possible values are
    -- eia1.medium, eia1.large, and eia1.xlarge.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateElasticInferenceAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'launchTemplateElasticInferenceAccelerator_count' - The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- 'type'', 'launchTemplateElasticInferenceAccelerator_type' - The type of elastic inference accelerator. The possible values are
-- eia1.medium, eia1.large, and eia1.xlarge.
newLaunchTemplateElasticInferenceAccelerator ::
  -- | 'type''
  Prelude.Text ->
  LaunchTemplateElasticInferenceAccelerator
newLaunchTemplateElasticInferenceAccelerator pType_ =
  LaunchTemplateElasticInferenceAccelerator'
    { count =
        Prelude.Nothing,
      type' = pType_
    }

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
launchTemplateElasticInferenceAccelerator_count :: Lens.Lens' LaunchTemplateElasticInferenceAccelerator (Prelude.Maybe Prelude.Natural)
launchTemplateElasticInferenceAccelerator_count = Lens.lens (\LaunchTemplateElasticInferenceAccelerator' {count} -> count) (\s@LaunchTemplateElasticInferenceAccelerator' {} a -> s {count = a} :: LaunchTemplateElasticInferenceAccelerator)

-- | The type of elastic inference accelerator. The possible values are
-- eia1.medium, eia1.large, and eia1.xlarge.
launchTemplateElasticInferenceAccelerator_type :: Lens.Lens' LaunchTemplateElasticInferenceAccelerator Prelude.Text
launchTemplateElasticInferenceAccelerator_type = Lens.lens (\LaunchTemplateElasticInferenceAccelerator' {type'} -> type') (\s@LaunchTemplateElasticInferenceAccelerator' {} a -> s {type' = a} :: LaunchTemplateElasticInferenceAccelerator)

instance
  Prelude.Hashable
    LaunchTemplateElasticInferenceAccelerator

instance
  Prelude.NFData
    LaunchTemplateElasticInferenceAccelerator

instance
  Prelude.ToQuery
    LaunchTemplateElasticInferenceAccelerator
  where
  toQuery
    LaunchTemplateElasticInferenceAccelerator' {..} =
      Prelude.mconcat
        ["Count" Prelude.=: count, "Type" Prelude.=: type']
