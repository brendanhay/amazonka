{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Updates settings for the require check in feature that are applied to a room profile. Require check in allows a meeting room’s Alexa or AVS device to prompt the user to check in; otherwise, the room will be released.
--
--
--
-- /See:/ 'updateRequireCheckIn' smart constructor.
data UpdateRequireCheckIn = UpdateRequireCheckIn'
  { _urciEnabled ::
      !(Maybe Bool),
    _urciReleaseAfterMinutes :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRequireCheckIn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urciEnabled' - Whether require check in is enabled or not.
--
-- * 'urciReleaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
updateRequireCheckIn ::
  UpdateRequireCheckIn
updateRequireCheckIn =
  UpdateRequireCheckIn'
    { _urciEnabled = Nothing,
      _urciReleaseAfterMinutes = Nothing
    }

-- | Whether require check in is enabled or not.
urciEnabled :: Lens' UpdateRequireCheckIn (Maybe Bool)
urciEnabled = lens _urciEnabled (\s a -> s {_urciEnabled = a})

-- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
urciReleaseAfterMinutes :: Lens' UpdateRequireCheckIn (Maybe Int)
urciReleaseAfterMinutes = lens _urciReleaseAfterMinutes (\s a -> s {_urciReleaseAfterMinutes = a})

instance Hashable UpdateRequireCheckIn

instance NFData UpdateRequireCheckIn

instance ToJSON UpdateRequireCheckIn where
  toJSON UpdateRequireCheckIn' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _urciEnabled,
            ("ReleaseAfterMinutes" .=) <$> _urciReleaseAfterMinutes
          ]
      )
