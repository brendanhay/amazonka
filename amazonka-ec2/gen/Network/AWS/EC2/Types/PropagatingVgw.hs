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
-- Module      : Network.AWS.EC2.Types.PropagatingVgw
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PropagatingVgw where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a virtual private gateway propagating route.
--
-- /See:/ 'newPropagatingVgw' smart constructor.
data PropagatingVgw = PropagatingVgw'
  { -- | The ID of the virtual private gateway.
    gatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PropagatingVgw' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayId', 'propagatingVgw_gatewayId' - The ID of the virtual private gateway.
newPropagatingVgw ::
  PropagatingVgw
newPropagatingVgw =
  PropagatingVgw' {gatewayId = Core.Nothing}

-- | The ID of the virtual private gateway.
propagatingVgw_gatewayId :: Lens.Lens' PropagatingVgw (Core.Maybe Core.Text)
propagatingVgw_gatewayId = Lens.lens (\PropagatingVgw' {gatewayId} -> gatewayId) (\s@PropagatingVgw' {} a -> s {gatewayId = a} :: PropagatingVgw)

instance Core.FromXML PropagatingVgw where
  parseXML x =
    PropagatingVgw' Core.<$> (x Core..@? "gatewayId")

instance Core.Hashable PropagatingVgw

instance Core.NFData PropagatingVgw
