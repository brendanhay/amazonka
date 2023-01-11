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
-- Module      : Amazonka.EC2.Types.VolumeDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an EBS volume.
--
-- /See:/ 'newVolumeDetail' smart constructor.
data VolumeDetail = VolumeDetail'
  { -- | The size of the volume, in GiB.
    size :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'volumeDetail_size' - The size of the volume, in GiB.
newVolumeDetail ::
  -- | 'size'
  Prelude.Integer ->
  VolumeDetail
newVolumeDetail pSize_ = VolumeDetail' {size = pSize_}

-- | The size of the volume, in GiB.
volumeDetail_size :: Lens.Lens' VolumeDetail Prelude.Integer
volumeDetail_size = Lens.lens (\VolumeDetail' {size} -> size) (\s@VolumeDetail' {} a -> s {size = a} :: VolumeDetail)

instance Prelude.Hashable VolumeDetail where
  hashWithSalt _salt VolumeDetail' {..} =
    _salt `Prelude.hashWithSalt` size

instance Prelude.NFData VolumeDetail where
  rnf VolumeDetail' {..} = Prelude.rnf size

instance Data.ToQuery VolumeDetail where
  toQuery VolumeDetail' {..} =
    Prelude.mconcat ["Size" Data.=: size]
