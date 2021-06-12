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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.MFAOptionType where

import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | /This data type is no longer supported./ You can use it only for SMS MFA
-- configurations. You can\'t use it for TOTP software token MFA
-- configurations.
--
-- /See:/ 'newMFAOptionType' smart constructor.
data MFAOptionType = MFAOptionType'
  { -- | The delivery medium to send the MFA code. You can use this parameter to
    -- set only the @SMS@ delivery medium value.
    deliveryMedium :: Core.Maybe DeliveryMediumType,
    -- | The attribute name of the MFA option type. The only valid value is
    -- @phone_number@.
    attributeName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MFAOptionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryMedium', 'mfaOptionType_deliveryMedium' - The delivery medium to send the MFA code. You can use this parameter to
-- set only the @SMS@ delivery medium value.
--
-- 'attributeName', 'mfaOptionType_attributeName' - The attribute name of the MFA option type. The only valid value is
-- @phone_number@.
newMFAOptionType ::
  MFAOptionType
newMFAOptionType =
  MFAOptionType'
    { deliveryMedium = Core.Nothing,
      attributeName = Core.Nothing
    }

-- | The delivery medium to send the MFA code. You can use this parameter to
-- set only the @SMS@ delivery medium value.
mfaOptionType_deliveryMedium :: Lens.Lens' MFAOptionType (Core.Maybe DeliveryMediumType)
mfaOptionType_deliveryMedium = Lens.lens (\MFAOptionType' {deliveryMedium} -> deliveryMedium) (\s@MFAOptionType' {} a -> s {deliveryMedium = a} :: MFAOptionType)

-- | The attribute name of the MFA option type. The only valid value is
-- @phone_number@.
mfaOptionType_attributeName :: Lens.Lens' MFAOptionType (Core.Maybe Core.Text)
mfaOptionType_attributeName = Lens.lens (\MFAOptionType' {attributeName} -> attributeName) (\s@MFAOptionType' {} a -> s {attributeName = a} :: MFAOptionType)

instance Core.FromJSON MFAOptionType where
  parseJSON =
    Core.withObject
      "MFAOptionType"
      ( \x ->
          MFAOptionType'
            Core.<$> (x Core..:? "DeliveryMedium")
            Core.<*> (x Core..:? "AttributeName")
      )

instance Core.Hashable MFAOptionType

instance Core.NFData MFAOptionType

instance Core.ToJSON MFAOptionType where
  toJSON MFAOptionType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeliveryMedium" Core..=) Core.<$> deliveryMedium,
            ("AttributeName" Core..=) Core.<$> attributeName
          ]
      )
