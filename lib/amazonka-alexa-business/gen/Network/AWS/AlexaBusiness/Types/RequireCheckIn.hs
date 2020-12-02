{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RequireCheckIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RequireCheckIn where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for the require check in feature that are applied to a room profile. Require check in allows a meeting room’s Alexa or AVS device to prompt the user to check in; otherwise, the room will be released.
--
--
--
-- /See:/ 'requireCheckIn' smart constructor.
data RequireCheckIn = RequireCheckIn'
  { _rciEnabled :: !(Maybe Bool),
    _rciReleaseAfterMinutes :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequireCheckIn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rciEnabled' - Whether require check in is enabled or not.
--
-- * 'rciReleaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
requireCheckIn ::
  RequireCheckIn
requireCheckIn =
  RequireCheckIn'
    { _rciEnabled = Nothing,
      _rciReleaseAfterMinutes = Nothing
    }

-- | Whether require check in is enabled or not.
rciEnabled :: Lens' RequireCheckIn (Maybe Bool)
rciEnabled = lens _rciEnabled (\s a -> s {_rciEnabled = a})

-- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
rciReleaseAfterMinutes :: Lens' RequireCheckIn (Maybe Int)
rciReleaseAfterMinutes = lens _rciReleaseAfterMinutes (\s a -> s {_rciReleaseAfterMinutes = a})

instance FromJSON RequireCheckIn where
  parseJSON =
    withObject
      "RequireCheckIn"
      ( \x ->
          RequireCheckIn'
            <$> (x .:? "Enabled") <*> (x .:? "ReleaseAfterMinutes")
      )

instance Hashable RequireCheckIn

instance NFData RequireCheckIn
