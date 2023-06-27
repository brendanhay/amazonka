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
-- Module      : Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptionsSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptionsSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a source or a destination.
--
-- /See:/ 'newRouteAnalysisEndpointOptionsSpecification' smart constructor.
data RouteAnalysisEndpointOptionsSpecification = RouteAnalysisEndpointOptionsSpecification'
  { -- | The IP address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the transit gateway attachment.
    transitGatewayAttachmentArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteAnalysisEndpointOptionsSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'routeAnalysisEndpointOptionsSpecification_ipAddress' - The IP address.
--
-- 'transitGatewayAttachmentArn', 'routeAnalysisEndpointOptionsSpecification_transitGatewayAttachmentArn' - The ARN of the transit gateway attachment.
newRouteAnalysisEndpointOptionsSpecification ::
  RouteAnalysisEndpointOptionsSpecification
newRouteAnalysisEndpointOptionsSpecification =
  RouteAnalysisEndpointOptionsSpecification'
    { ipAddress =
        Prelude.Nothing,
      transitGatewayAttachmentArn =
        Prelude.Nothing
    }

-- | The IP address.
routeAnalysisEndpointOptionsSpecification_ipAddress :: Lens.Lens' RouteAnalysisEndpointOptionsSpecification (Prelude.Maybe Prelude.Text)
routeAnalysisEndpointOptionsSpecification_ipAddress = Lens.lens (\RouteAnalysisEndpointOptionsSpecification' {ipAddress} -> ipAddress) (\s@RouteAnalysisEndpointOptionsSpecification' {} a -> s {ipAddress = a} :: RouteAnalysisEndpointOptionsSpecification)

-- | The ARN of the transit gateway attachment.
routeAnalysisEndpointOptionsSpecification_transitGatewayAttachmentArn :: Lens.Lens' RouteAnalysisEndpointOptionsSpecification (Prelude.Maybe Prelude.Text)
routeAnalysisEndpointOptionsSpecification_transitGatewayAttachmentArn = Lens.lens (\RouteAnalysisEndpointOptionsSpecification' {transitGatewayAttachmentArn} -> transitGatewayAttachmentArn) (\s@RouteAnalysisEndpointOptionsSpecification' {} a -> s {transitGatewayAttachmentArn = a} :: RouteAnalysisEndpointOptionsSpecification)

instance
  Prelude.Hashable
    RouteAnalysisEndpointOptionsSpecification
  where
  hashWithSalt
    _salt
    RouteAnalysisEndpointOptionsSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` ipAddress
        `Prelude.hashWithSalt` transitGatewayAttachmentArn

instance
  Prelude.NFData
    RouteAnalysisEndpointOptionsSpecification
  where
  rnf RouteAnalysisEndpointOptionsSpecification' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentArn

instance
  Data.ToJSON
    RouteAnalysisEndpointOptionsSpecification
  where
  toJSON RouteAnalysisEndpointOptionsSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IpAddress" Data..=) Prelude.<$> ipAddress,
            ("TransitGatewayAttachmentArn" Data..=)
              Prelude.<$> transitGatewayAttachmentArn
          ]
      )
