{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FollowPoint
import Network.AWS.Prelude

-- | Settings to specify if an action follows another.
--
-- /See:/ 'followModeScheduleActionStartSettings' smart constructor.
data FollowModeScheduleActionStartSettings = FollowModeScheduleActionStartSettings'
  { _fmsassReferenceActionName ::
      !Text,
    _fmsassFollowPoint ::
      !FollowPoint
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FollowModeScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmsassReferenceActionName' - The action name of another action that this one refers to.
--
-- * 'fmsassFollowPoint' - Identifies whether this action starts relative to the start or relative to the end of the reference action.
followModeScheduleActionStartSettings ::
  -- | 'fmsassReferenceActionName'
  Text ->
  -- | 'fmsassFollowPoint'
  FollowPoint ->
  FollowModeScheduleActionStartSettings
followModeScheduleActionStartSettings
  pReferenceActionName_
  pFollowPoint_ =
    FollowModeScheduleActionStartSettings'
      { _fmsassReferenceActionName =
          pReferenceActionName_,
        _fmsassFollowPoint = pFollowPoint_
      }

-- | The action name of another action that this one refers to.
fmsassReferenceActionName :: Lens' FollowModeScheduleActionStartSettings Text
fmsassReferenceActionName = lens _fmsassReferenceActionName (\s a -> s {_fmsassReferenceActionName = a})

-- | Identifies whether this action starts relative to the start or relative to the end of the reference action.
fmsassFollowPoint :: Lens' FollowModeScheduleActionStartSettings FollowPoint
fmsassFollowPoint = lens _fmsassFollowPoint (\s a -> s {_fmsassFollowPoint = a})

instance FromJSON FollowModeScheduleActionStartSettings where
  parseJSON =
    withObject
      "FollowModeScheduleActionStartSettings"
      ( \x ->
          FollowModeScheduleActionStartSettings'
            <$> (x .: "referenceActionName") <*> (x .: "followPoint")
      )

instance Hashable FollowModeScheduleActionStartSettings

instance NFData FollowModeScheduleActionStartSettings

instance ToJSON FollowModeScheduleActionStartSettings where
  toJSON FollowModeScheduleActionStartSettings' {..} =
    object
      ( catMaybes
          [ Just ("referenceActionName" .= _fmsassReferenceActionName),
            Just ("followPoint" .= _fmsassFollowPoint)
          ]
      )
