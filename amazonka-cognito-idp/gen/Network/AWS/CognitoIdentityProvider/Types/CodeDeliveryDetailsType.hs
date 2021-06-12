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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType where

import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The code delivery details being returned from the server.
--
-- /See:/ 'newCodeDeliveryDetailsType' smart constructor.
data CodeDeliveryDetailsType = CodeDeliveryDetailsType'
  { -- | The delivery medium (email message or phone number).
    deliveryMedium :: Core.Maybe DeliveryMediumType,
    -- | The attribute name.
    attributeName :: Core.Maybe Core.Text,
    -- | The destination for the code delivery details.
    destination :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeDeliveryDetailsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryMedium', 'codeDeliveryDetailsType_deliveryMedium' - The delivery medium (email message or phone number).
--
-- 'attributeName', 'codeDeliveryDetailsType_attributeName' - The attribute name.
--
-- 'destination', 'codeDeliveryDetailsType_destination' - The destination for the code delivery details.
newCodeDeliveryDetailsType ::
  CodeDeliveryDetailsType
newCodeDeliveryDetailsType =
  CodeDeliveryDetailsType'
    { deliveryMedium =
        Core.Nothing,
      attributeName = Core.Nothing,
      destination = Core.Nothing
    }

-- | The delivery medium (email message or phone number).
codeDeliveryDetailsType_deliveryMedium :: Lens.Lens' CodeDeliveryDetailsType (Core.Maybe DeliveryMediumType)
codeDeliveryDetailsType_deliveryMedium = Lens.lens (\CodeDeliveryDetailsType' {deliveryMedium} -> deliveryMedium) (\s@CodeDeliveryDetailsType' {} a -> s {deliveryMedium = a} :: CodeDeliveryDetailsType)

-- | The attribute name.
codeDeliveryDetailsType_attributeName :: Lens.Lens' CodeDeliveryDetailsType (Core.Maybe Core.Text)
codeDeliveryDetailsType_attributeName = Lens.lens (\CodeDeliveryDetailsType' {attributeName} -> attributeName) (\s@CodeDeliveryDetailsType' {} a -> s {attributeName = a} :: CodeDeliveryDetailsType)

-- | The destination for the code delivery details.
codeDeliveryDetailsType_destination :: Lens.Lens' CodeDeliveryDetailsType (Core.Maybe Core.Text)
codeDeliveryDetailsType_destination = Lens.lens (\CodeDeliveryDetailsType' {destination} -> destination) (\s@CodeDeliveryDetailsType' {} a -> s {destination = a} :: CodeDeliveryDetailsType)

instance Core.FromJSON CodeDeliveryDetailsType where
  parseJSON =
    Core.withObject
      "CodeDeliveryDetailsType"
      ( \x ->
          CodeDeliveryDetailsType'
            Core.<$> (x Core..:? "DeliveryMedium")
            Core.<*> (x Core..:? "AttributeName")
            Core.<*> (x Core..:? "Destination")
      )

instance Core.Hashable CodeDeliveryDetailsType

instance Core.NFData CodeDeliveryDetailsType
