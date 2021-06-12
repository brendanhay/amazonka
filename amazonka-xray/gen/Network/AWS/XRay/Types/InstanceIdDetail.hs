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
-- Module      : Network.AWS.XRay.Types.InstanceIdDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InstanceIdDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of EC2 instance IDs corresponding to the segments in a trace.
--
-- /See:/ 'newInstanceIdDetail' smart constructor.
data InstanceIdDetail = InstanceIdDetail'
  { -- | The ID of a corresponding EC2 instance.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceIdDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'instanceIdDetail_id' - The ID of a corresponding EC2 instance.
newInstanceIdDetail ::
  InstanceIdDetail
newInstanceIdDetail =
  InstanceIdDetail' {id = Core.Nothing}

-- | The ID of a corresponding EC2 instance.
instanceIdDetail_id :: Lens.Lens' InstanceIdDetail (Core.Maybe Core.Text)
instanceIdDetail_id = Lens.lens (\InstanceIdDetail' {id} -> id) (\s@InstanceIdDetail' {} a -> s {id = a} :: InstanceIdDetail)

instance Core.FromJSON InstanceIdDetail where
  parseJSON =
    Core.withObject
      "InstanceIdDetail"
      (\x -> InstanceIdDetail' Core.<$> (x Core..:? "Id"))

instance Core.Hashable InstanceIdDetail

instance Core.NFData InstanceIdDetail
