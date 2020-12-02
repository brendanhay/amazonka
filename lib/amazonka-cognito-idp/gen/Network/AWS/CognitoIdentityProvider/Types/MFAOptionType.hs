{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.MFAOptionType where

import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /This data type is no longer supported./ You can use it only for SMS MFA configurations. You can't use it for TOTP software token MFA configurations.
--
--
--
-- /See:/ 'mfaOptionType' smart constructor.
data MFAOptionType = MFAOptionType'
  { _motDeliveryMedium ::
      !(Maybe DeliveryMediumType),
    _motAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MFAOptionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'motDeliveryMedium' - The delivery medium to send the MFA code. You can use this parameter to set only the @SMS@ delivery medium value.
--
-- * 'motAttributeName' - The attribute name of the MFA option type. The only valid value is @phone_number@ .
mfaOptionType ::
  MFAOptionType
mfaOptionType =
  MFAOptionType'
    { _motDeliveryMedium = Nothing,
      _motAttributeName = Nothing
    }

-- | The delivery medium to send the MFA code. You can use this parameter to set only the @SMS@ delivery medium value.
motDeliveryMedium :: Lens' MFAOptionType (Maybe DeliveryMediumType)
motDeliveryMedium = lens _motDeliveryMedium (\s a -> s {_motDeliveryMedium = a})

-- | The attribute name of the MFA option type. The only valid value is @phone_number@ .
motAttributeName :: Lens' MFAOptionType (Maybe Text)
motAttributeName = lens _motAttributeName (\s a -> s {_motAttributeName = a})

instance FromJSON MFAOptionType where
  parseJSON =
    withObject
      "MFAOptionType"
      ( \x ->
          MFAOptionType'
            <$> (x .:? "DeliveryMedium") <*> (x .:? "AttributeName")
      )

instance Hashable MFAOptionType

instance NFData MFAOptionType

instance ToJSON MFAOptionType where
  toJSON MFAOptionType' {..} =
    object
      ( catMaybes
          [ ("DeliveryMedium" .=) <$> _motDeliveryMedium,
            ("AttributeName" .=) <$> _motAttributeName
          ]
      )
