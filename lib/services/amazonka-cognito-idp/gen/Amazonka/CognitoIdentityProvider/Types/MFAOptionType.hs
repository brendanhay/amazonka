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
-- Module      : Amazonka.CognitoIdentityProvider.Types.MFAOptionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.MFAOptionType where

import Amazonka.CognitoIdentityProvider.Types.DeliveryMediumType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | /This data type is no longer supported./ Applies only to SMS
-- multi-factor authentication (MFA) configurations. Does not apply to
-- time-based one-time password (TOTP) software token MFA configurations.
--
-- /See:/ 'newMFAOptionType' smart constructor.
data MFAOptionType = MFAOptionType'
  { -- | The delivery medium to send the MFA code. You can use this parameter to
    -- set only the @SMS@ delivery medium value.
    deliveryMedium :: Prelude.Maybe DeliveryMediumType,
    -- | The attribute name of the MFA option type. The only valid value is
    -- @phone_number@.
    attributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { deliveryMedium = Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The delivery medium to send the MFA code. You can use this parameter to
-- set only the @SMS@ delivery medium value.
mfaOptionType_deliveryMedium :: Lens.Lens' MFAOptionType (Prelude.Maybe DeliveryMediumType)
mfaOptionType_deliveryMedium = Lens.lens (\MFAOptionType' {deliveryMedium} -> deliveryMedium) (\s@MFAOptionType' {} a -> s {deliveryMedium = a} :: MFAOptionType)

-- | The attribute name of the MFA option type. The only valid value is
-- @phone_number@.
mfaOptionType_attributeName :: Lens.Lens' MFAOptionType (Prelude.Maybe Prelude.Text)
mfaOptionType_attributeName = Lens.lens (\MFAOptionType' {attributeName} -> attributeName) (\s@MFAOptionType' {} a -> s {attributeName = a} :: MFAOptionType)

instance Core.FromJSON MFAOptionType where
  parseJSON =
    Core.withObject
      "MFAOptionType"
      ( \x ->
          MFAOptionType'
            Prelude.<$> (x Core..:? "DeliveryMedium")
            Prelude.<*> (x Core..:? "AttributeName")
      )

instance Prelude.Hashable MFAOptionType where
  hashWithSalt _salt MFAOptionType' {..} =
    _salt `Prelude.hashWithSalt` deliveryMedium
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData MFAOptionType where
  rnf MFAOptionType' {..} =
    Prelude.rnf deliveryMedium
      `Prelude.seq` Prelude.rnf attributeName

instance Core.ToJSON MFAOptionType where
  toJSON MFAOptionType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeliveryMedium" Core..=)
              Prelude.<$> deliveryMedium,
            ("AttributeName" Core..=) Prelude.<$> attributeName
          ]
      )
