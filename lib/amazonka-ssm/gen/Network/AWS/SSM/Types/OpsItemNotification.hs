{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemNotification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A notification about the OpsItem.
--
--
--
-- /See:/ 'opsItemNotification' smart constructor.
newtype OpsItemNotification = OpsItemNotification'
  { _oinARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsItemNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oinARN' - The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
opsItemNotification ::
  OpsItemNotification
opsItemNotification = OpsItemNotification' {_oinARN = Nothing}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
oinARN :: Lens' OpsItemNotification (Maybe Text)
oinARN = lens _oinARN (\s a -> s {_oinARN = a})

instance FromJSON OpsItemNotification where
  parseJSON =
    withObject
      "OpsItemNotification"
      (\x -> OpsItemNotification' <$> (x .:? "Arn"))

instance Hashable OpsItemNotification

instance NFData OpsItemNotification

instance ToJSON OpsItemNotification where
  toJSON OpsItemNotification' {..} =
    object (catMaybes [("Arn" .=) <$> _oinARN])
