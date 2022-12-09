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
-- Module      : Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a source or a destination.
--
-- /See:/ 'newRouteAnalysisEndpointOptions' smart constructor.
data RouteAnalysisEndpointOptions = RouteAnalysisEndpointOptions'
  { -- | The IP address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the transit gateway.
    transitGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the transit gateway attachment.
    transitGatewayAttachmentArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteAnalysisEndpointOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'routeAnalysisEndpointOptions_ipAddress' - The IP address.
--
-- 'transitGatewayArn', 'routeAnalysisEndpointOptions_transitGatewayArn' - The ARN of the transit gateway.
--
-- 'transitGatewayAttachmentArn', 'routeAnalysisEndpointOptions_transitGatewayAttachmentArn' - The ARN of the transit gateway attachment.
newRouteAnalysisEndpointOptions ::
  RouteAnalysisEndpointOptions
newRouteAnalysisEndpointOptions =
  RouteAnalysisEndpointOptions'
    { ipAddress =
        Prelude.Nothing,
      transitGatewayArn = Prelude.Nothing,
      transitGatewayAttachmentArn = Prelude.Nothing
    }

-- | The IP address.
routeAnalysisEndpointOptions_ipAddress :: Lens.Lens' RouteAnalysisEndpointOptions (Prelude.Maybe Prelude.Text)
routeAnalysisEndpointOptions_ipAddress = Lens.lens (\RouteAnalysisEndpointOptions' {ipAddress} -> ipAddress) (\s@RouteAnalysisEndpointOptions' {} a -> s {ipAddress = a} :: RouteAnalysisEndpointOptions)

-- | The ARN of the transit gateway.
routeAnalysisEndpointOptions_transitGatewayArn :: Lens.Lens' RouteAnalysisEndpointOptions (Prelude.Maybe Prelude.Text)
routeAnalysisEndpointOptions_transitGatewayArn = Lens.lens (\RouteAnalysisEndpointOptions' {transitGatewayArn} -> transitGatewayArn) (\s@RouteAnalysisEndpointOptions' {} a -> s {transitGatewayArn = a} :: RouteAnalysisEndpointOptions)

-- | The ARN of the transit gateway attachment.
routeAnalysisEndpointOptions_transitGatewayAttachmentArn :: Lens.Lens' RouteAnalysisEndpointOptions (Prelude.Maybe Prelude.Text)
routeAnalysisEndpointOptions_transitGatewayAttachmentArn = Lens.lens (\RouteAnalysisEndpointOptions' {transitGatewayAttachmentArn} -> transitGatewayAttachmentArn) (\s@RouteAnalysisEndpointOptions' {} a -> s {transitGatewayAttachmentArn = a} :: RouteAnalysisEndpointOptions)

instance Data.FromJSON RouteAnalysisEndpointOptions where
  parseJSON =
    Data.withObject
      "RouteAnalysisEndpointOptions"
      ( \x ->
          RouteAnalysisEndpointOptions'
            Prelude.<$> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "TransitGatewayArn")
            Prelude.<*> (x Data..:? "TransitGatewayAttachmentArn")
      )

instance
  Prelude.Hashable
    RouteAnalysisEndpointOptions
  where
  hashWithSalt _salt RouteAnalysisEndpointOptions' {..} =
    _salt `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` transitGatewayArn
      `Prelude.hashWithSalt` transitGatewayAttachmentArn

instance Prelude.NFData RouteAnalysisEndpointOptions where
  rnf RouteAnalysisEndpointOptions' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf transitGatewayArn
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentArn
