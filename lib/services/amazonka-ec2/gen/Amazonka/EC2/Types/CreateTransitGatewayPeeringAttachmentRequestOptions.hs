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
-- Module      : Amazonka.EC2.Types.CreateTransitGatewayPeeringAttachmentRequestOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateTransitGatewayPeeringAttachmentRequestOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DynamicRoutingValue
import qualified Amazonka.Prelude as Prelude

-- | Describes whether dynamic routing is enabled or disabled for the transit
-- gateway peering request.
--
-- /See:/ 'newCreateTransitGatewayPeeringAttachmentRequestOptions' smart constructor.
data CreateTransitGatewayPeeringAttachmentRequestOptions = CreateTransitGatewayPeeringAttachmentRequestOptions'
  { -- | Indicates whether dynamic routing is enabled or disabled.
    dynamicRouting :: Prelude.Maybe DynamicRoutingValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPeeringAttachmentRequestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamicRouting', 'createTransitGatewayPeeringAttachmentRequestOptions_dynamicRouting' - Indicates whether dynamic routing is enabled or disabled.
newCreateTransitGatewayPeeringAttachmentRequestOptions ::
  CreateTransitGatewayPeeringAttachmentRequestOptions
newCreateTransitGatewayPeeringAttachmentRequestOptions =
  CreateTransitGatewayPeeringAttachmentRequestOptions'
    { dynamicRouting =
        Prelude.Nothing
    }

-- | Indicates whether dynamic routing is enabled or disabled.
createTransitGatewayPeeringAttachmentRequestOptions_dynamicRouting :: Lens.Lens' CreateTransitGatewayPeeringAttachmentRequestOptions (Prelude.Maybe DynamicRoutingValue)
createTransitGatewayPeeringAttachmentRequestOptions_dynamicRouting = Lens.lens (\CreateTransitGatewayPeeringAttachmentRequestOptions' {dynamicRouting} -> dynamicRouting) (\s@CreateTransitGatewayPeeringAttachmentRequestOptions' {} a -> s {dynamicRouting = a} :: CreateTransitGatewayPeeringAttachmentRequestOptions)

instance
  Prelude.Hashable
    CreateTransitGatewayPeeringAttachmentRequestOptions
  where
  hashWithSalt
    _salt
    CreateTransitGatewayPeeringAttachmentRequestOptions' {..} =
      _salt `Prelude.hashWithSalt` dynamicRouting

instance
  Prelude.NFData
    CreateTransitGatewayPeeringAttachmentRequestOptions
  where
  rnf
    CreateTransitGatewayPeeringAttachmentRequestOptions' {..} =
      Prelude.rnf dynamicRouting

instance
  Data.ToQuery
    CreateTransitGatewayPeeringAttachmentRequestOptions
  where
  toQuery
    CreateTransitGatewayPeeringAttachmentRequestOptions' {..} =
      Prelude.mconcat
        ["DynamicRouting" Data.=: dynamicRouting]
