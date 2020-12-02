{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType where

import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The code delivery details being returned from the server.
--
--
--
-- /See:/ 'codeDeliveryDetailsType' smart constructor.
data CodeDeliveryDetailsType = CodeDeliveryDetailsType'
  { _cddtDestination ::
      !(Maybe Text),
    _cddtDeliveryMedium ::
      !(Maybe DeliveryMediumType),
    _cddtAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeDeliveryDetailsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cddtDestination' - The destination for the code delivery details.
--
-- * 'cddtDeliveryMedium' - The delivery medium (email message or phone number).
--
-- * 'cddtAttributeName' - The attribute name.
codeDeliveryDetailsType ::
  CodeDeliveryDetailsType
codeDeliveryDetailsType =
  CodeDeliveryDetailsType'
    { _cddtDestination = Nothing,
      _cddtDeliveryMedium = Nothing,
      _cddtAttributeName = Nothing
    }

-- | The destination for the code delivery details.
cddtDestination :: Lens' CodeDeliveryDetailsType (Maybe Text)
cddtDestination = lens _cddtDestination (\s a -> s {_cddtDestination = a})

-- | The delivery medium (email message or phone number).
cddtDeliveryMedium :: Lens' CodeDeliveryDetailsType (Maybe DeliveryMediumType)
cddtDeliveryMedium = lens _cddtDeliveryMedium (\s a -> s {_cddtDeliveryMedium = a})

-- | The attribute name.
cddtAttributeName :: Lens' CodeDeliveryDetailsType (Maybe Text)
cddtAttributeName = lens _cddtAttributeName (\s a -> s {_cddtAttributeName = a})

instance FromJSON CodeDeliveryDetailsType where
  parseJSON =
    withObject
      "CodeDeliveryDetailsType"
      ( \x ->
          CodeDeliveryDetailsType'
            <$> (x .:? "Destination")
            <*> (x .:? "DeliveryMedium")
            <*> (x .:? "AttributeName")
      )

instance Hashable CodeDeliveryDetailsType

instance NFData CodeDeliveryDetailsType
