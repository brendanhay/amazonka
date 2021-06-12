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
-- Module      : Network.AWS.Redshift.Types.VpcEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.VpcEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | The connection endpoint for connecting an Amazon Redshift cluster
-- through the proxy.
--
-- /See:/ 'newVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { -- | The connection endpoint ID for connecting an Amazon Redshift cluster
    -- through the proxy.
    vpcEndpointId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcEndpointId', 'vpcEndpoint_vpcEndpointId' - The connection endpoint ID for connecting an Amazon Redshift cluster
-- through the proxy.
newVpcEndpoint ::
  VpcEndpoint
newVpcEndpoint =
  VpcEndpoint' {vpcEndpointId = Core.Nothing}

-- | The connection endpoint ID for connecting an Amazon Redshift cluster
-- through the proxy.
vpcEndpoint_vpcEndpointId :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
vpcEndpoint_vpcEndpointId = Lens.lens (\VpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpoint' {} a -> s {vpcEndpointId = a} :: VpcEndpoint)

instance Core.FromXML VpcEndpoint where
  parseXML x =
    VpcEndpoint' Core.<$> (x Core..@? "VpcEndpointId")

instance Core.Hashable VpcEndpoint

instance Core.NFData VpcEndpoint
