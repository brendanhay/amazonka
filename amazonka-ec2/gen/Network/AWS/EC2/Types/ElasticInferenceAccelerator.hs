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
import qualified Network.AWS.Prelude as Prelude

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'newElasticInferenceAccelerator' smart constructor.
data ElasticInferenceAccelerator = ElasticInferenceAccelerator'
  { -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Prelude.Maybe Prelude.Natural,
    -- | The type of elastic inference accelerator. The possible values are
    -- @eia1.medium@, @eia1.large@, @eia1.xlarge@, @eia2.medium@, @eia2.large@,
    -- and @eia2.xlarge@.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ElasticInferenceAccelerator
newElasticInferenceAccelerator pType_ =
  ElasticInferenceAccelerator'
    { count =
        Prelude.Nothing,
      type' = pType_
    }

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
elasticInferenceAccelerator_count :: Lens.Lens' ElasticInferenceAccelerator (Prelude.Maybe Prelude.Natural)
elasticInferenceAccelerator_count = Lens.lens (\ElasticInferenceAccelerator' {count} -> count) (\s@ElasticInferenceAccelerator' {} a -> s {count = a} :: ElasticInferenceAccelerator)

-- | The type of elastic inference accelerator. The possible values are
-- @eia1.medium@, @eia1.large@, @eia1.xlarge@, @eia2.medium@, @eia2.large@,
-- and @eia2.xlarge@.
elasticInferenceAccelerator_type :: Lens.Lens' ElasticInferenceAccelerator Prelude.Text
elasticInferenceAccelerator_type = Lens.lens (\ElasticInferenceAccelerator' {type'} -> type') (\s@ElasticInferenceAccelerator' {} a -> s {type' = a} :: ElasticInferenceAccelerator)

instance Prelude.Hashable ElasticInferenceAccelerator

instance Prelude.NFData ElasticInferenceAccelerator

instance Core.ToQuery ElasticInferenceAccelerator where
  toQuery ElasticInferenceAccelerator' {..} =
    Prelude.mconcat
      ["Count" Core.=: count, "Type" Core.=: type']
