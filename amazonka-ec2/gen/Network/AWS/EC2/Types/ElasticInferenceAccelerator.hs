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
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticInferenceAccelerator where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'newElasticInferenceAccelerator' smart constructor.
data ElasticInferenceAccelerator = ElasticInferenceAccelerator'
  { -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Core.Maybe Core.Natural,
    -- | The type of elastic inference accelerator. The possible values are
    -- @eia1.medium@, @eia1.large@, @eia1.xlarge@, @eia2.medium@, @eia2.large@,
    -- and @eia2.xlarge@.
    type' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticInferenceAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'elasticInferenceAccelerator_count' - The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- 'type'', 'elasticInferenceAccelerator_type' - The type of elastic inference accelerator. The possible values are
-- @eia1.medium@, @eia1.large@, @eia1.xlarge@, @eia2.medium@, @eia2.large@,
-- and @eia2.xlarge@.
newElasticInferenceAccelerator ::
  -- | 'type''
  Core.Text ->
  ElasticInferenceAccelerator
newElasticInferenceAccelerator pType_ =
  ElasticInferenceAccelerator'
    { count = Core.Nothing,
      type' = pType_
    }

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
elasticInferenceAccelerator_count :: Lens.Lens' ElasticInferenceAccelerator (Core.Maybe Core.Natural)
elasticInferenceAccelerator_count = Lens.lens (\ElasticInferenceAccelerator' {count} -> count) (\s@ElasticInferenceAccelerator' {} a -> s {count = a} :: ElasticInferenceAccelerator)

-- | The type of elastic inference accelerator. The possible values are
-- @eia1.medium@, @eia1.large@, @eia1.xlarge@, @eia2.medium@, @eia2.large@,
-- and @eia2.xlarge@.
elasticInferenceAccelerator_type :: Lens.Lens' ElasticInferenceAccelerator Core.Text
elasticInferenceAccelerator_type = Lens.lens (\ElasticInferenceAccelerator' {type'} -> type') (\s@ElasticInferenceAccelerator' {} a -> s {type' = a} :: ElasticInferenceAccelerator)

instance Core.Hashable ElasticInferenceAccelerator

instance Core.NFData ElasticInferenceAccelerator

instance Core.ToQuery ElasticInferenceAccelerator where
  toQuery ElasticInferenceAccelerator' {..} =
    Core.mconcat
      ["Count" Core.=: count, "Type" Core.=: type']
