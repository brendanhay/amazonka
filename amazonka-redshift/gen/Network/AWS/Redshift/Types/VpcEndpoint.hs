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
-- Module      : Network.AWS.Redshift.Types.VpcEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.VpcEndpoint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | The connection endpoint for connecting an Amazon Redshift cluster
-- through the proxy.
--
-- /See:/ 'newVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { -- | The connection endpoint ID for connecting an Amazon Redshift cluster
    -- through the proxy.
    vpcEndpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  VpcEndpoint' {vpcEndpointId = Prelude.Nothing}

-- | The connection endpoint ID for connecting an Amazon Redshift cluster
-- through the proxy.
vpcEndpoint_vpcEndpointId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcEndpointId = Lens.lens (\VpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpoint' {} a -> s {vpcEndpointId = a} :: VpcEndpoint)

instance Prelude.FromXML VpcEndpoint where
  parseXML x =
    VpcEndpoint'
      Prelude.<$> (x Prelude..@? "VpcEndpointId")

instance Prelude.Hashable VpcEndpoint

instance Prelude.NFData VpcEndpoint
