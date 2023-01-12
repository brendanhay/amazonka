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
-- Module      : Amazonka.EC2.Types.TransitGatewayPeeringAttachmentOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPeeringAttachmentOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DynamicRoutingValue
import qualified Amazonka.Prelude as Prelude

-- | Describes dynamic routing for the transit gateway peering attachment.
--
-- /See:/ 'newTransitGatewayPeeringAttachmentOptions' smart constructor.
data TransitGatewayPeeringAttachmentOptions = TransitGatewayPeeringAttachmentOptions'
  { -- | Describes whether dynamic routing is enabled or disabled for the transit
    -- gateway peering attachment.
    dynamicRouting :: Prelude.Maybe DynamicRoutingValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayPeeringAttachmentOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamicRouting', 'transitGatewayPeeringAttachmentOptions_dynamicRouting' - Describes whether dynamic routing is enabled or disabled for the transit
-- gateway peering attachment.
newTransitGatewayPeeringAttachmentOptions ::
  TransitGatewayPeeringAttachmentOptions
newTransitGatewayPeeringAttachmentOptions =
  TransitGatewayPeeringAttachmentOptions'
    { dynamicRouting =
        Prelude.Nothing
    }

-- | Describes whether dynamic routing is enabled or disabled for the transit
-- gateway peering attachment.
transitGatewayPeeringAttachmentOptions_dynamicRouting :: Lens.Lens' TransitGatewayPeeringAttachmentOptions (Prelude.Maybe DynamicRoutingValue)
transitGatewayPeeringAttachmentOptions_dynamicRouting = Lens.lens (\TransitGatewayPeeringAttachmentOptions' {dynamicRouting} -> dynamicRouting) (\s@TransitGatewayPeeringAttachmentOptions' {} a -> s {dynamicRouting = a} :: TransitGatewayPeeringAttachmentOptions)

instance
  Data.FromXML
    TransitGatewayPeeringAttachmentOptions
  where
  parseXML x =
    TransitGatewayPeeringAttachmentOptions'
      Prelude.<$> (x Data..@? "dynamicRouting")

instance
  Prelude.Hashable
    TransitGatewayPeeringAttachmentOptions
  where
  hashWithSalt
    _salt
    TransitGatewayPeeringAttachmentOptions' {..} =
      _salt `Prelude.hashWithSalt` dynamicRouting

instance
  Prelude.NFData
    TransitGatewayPeeringAttachmentOptions
  where
  rnf TransitGatewayPeeringAttachmentOptions' {..} =
    Prelude.rnf dynamicRouting
