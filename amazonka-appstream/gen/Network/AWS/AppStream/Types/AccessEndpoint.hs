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
-- Module      : Network.AWS.AppStream.Types.AccessEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.AccessEndpoint where

import Network.AWS.AppStream.Types.AccessEndpointType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an interface VPC endpoint (interface endpoint) that lets you
-- create a private connection between the virtual private cloud (VPC) that
-- you specify and AppStream 2.0. When you specify an interface endpoint
-- for a stack, users of the stack can connect to AppStream 2.0 only
-- through that endpoint. When you specify an interface endpoint for an
-- image builder, administrators can connect to the image builder only
-- through that endpoint.
--
-- /See:/ 'newAccessEndpoint' smart constructor.
data AccessEndpoint = AccessEndpoint'
  { -- | The identifier (ID) of the VPC in which the interface endpoint is used.
    vpceId :: Prelude.Maybe Prelude.Text,
    -- | The type of interface endpoint.
    endpointType :: AccessEndpointType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccessEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceId', 'accessEndpoint_vpceId' - The identifier (ID) of the VPC in which the interface endpoint is used.
--
-- 'endpointType', 'accessEndpoint_endpointType' - The type of interface endpoint.
newAccessEndpoint ::
  -- | 'endpointType'
  AccessEndpointType ->
  AccessEndpoint
newAccessEndpoint pEndpointType_ =
  AccessEndpoint'
    { vpceId = Prelude.Nothing,
      endpointType = pEndpointType_
    }

-- | The identifier (ID) of the VPC in which the interface endpoint is used.
accessEndpoint_vpceId :: Lens.Lens' AccessEndpoint (Prelude.Maybe Prelude.Text)
accessEndpoint_vpceId = Lens.lens (\AccessEndpoint' {vpceId} -> vpceId) (\s@AccessEndpoint' {} a -> s {vpceId = a} :: AccessEndpoint)

-- | The type of interface endpoint.
accessEndpoint_endpointType :: Lens.Lens' AccessEndpoint AccessEndpointType
accessEndpoint_endpointType = Lens.lens (\AccessEndpoint' {endpointType} -> endpointType) (\s@AccessEndpoint' {} a -> s {endpointType = a} :: AccessEndpoint)

instance Prelude.FromJSON AccessEndpoint where
  parseJSON =
    Prelude.withObject
      "AccessEndpoint"
      ( \x ->
          AccessEndpoint'
            Prelude.<$> (x Prelude..:? "VpceId")
            Prelude.<*> (x Prelude..: "EndpointType")
      )

instance Prelude.Hashable AccessEndpoint

instance Prelude.NFData AccessEndpoint

instance Prelude.ToJSON AccessEndpoint where
  toJSON AccessEndpoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("VpceId" Prelude..=) Prelude.<$> vpceId,
            Prelude.Just
              ("EndpointType" Prelude..= endpointType)
          ]
      )
