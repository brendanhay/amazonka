{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTypeInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.RegistrationStatus

-- | Detailed information about an activity type.
--
--
--
-- /See:/ 'activityTypeInfo' smart constructor.
data ActivityTypeInfo = ActivityTypeInfo'
  { _atiDeprecationDate ::
      !(Maybe POSIX),
    _atiDescription :: !(Maybe Text),
    _atiActivityType :: !ActivityType,
    _atiStatus :: !RegistrationStatus,
    _atiCreationDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTypeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atiDeprecationDate' - If DEPRECATED, the date and time 'DeprecateActivityType' was called.
--
-- * 'atiDescription' - The description of the activity type provided in 'RegisterActivityType' .
--
-- * 'atiActivityType' - The 'ActivityType' type structure representing the activity type.
--
-- * 'atiStatus' - The current status of the activity type.
--
-- * 'atiCreationDate' - The date and time this activity type was created through 'RegisterActivityType' .
activityTypeInfo ::
  -- | 'atiActivityType'
  ActivityType ->
  -- | 'atiStatus'
  RegistrationStatus ->
  -- | 'atiCreationDate'
  UTCTime ->
  ActivityTypeInfo
activityTypeInfo pActivityType_ pStatus_ pCreationDate_ =
  ActivityTypeInfo'
    { _atiDeprecationDate = Nothing,
      _atiDescription = Nothing,
      _atiActivityType = pActivityType_,
      _atiStatus = pStatus_,
      _atiCreationDate = _Time # pCreationDate_
    }

-- | If DEPRECATED, the date and time 'DeprecateActivityType' was called.
atiDeprecationDate :: Lens' ActivityTypeInfo (Maybe UTCTime)
atiDeprecationDate = lens _atiDeprecationDate (\s a -> s {_atiDeprecationDate = a}) . mapping _Time

-- | The description of the activity type provided in 'RegisterActivityType' .
atiDescription :: Lens' ActivityTypeInfo (Maybe Text)
atiDescription = lens _atiDescription (\s a -> s {_atiDescription = a})

-- | The 'ActivityType' type structure representing the activity type.
atiActivityType :: Lens' ActivityTypeInfo ActivityType
atiActivityType = lens _atiActivityType (\s a -> s {_atiActivityType = a})

-- | The current status of the activity type.
atiStatus :: Lens' ActivityTypeInfo RegistrationStatus
atiStatus = lens _atiStatus (\s a -> s {_atiStatus = a})

-- | The date and time this activity type was created through 'RegisterActivityType' .
atiCreationDate :: Lens' ActivityTypeInfo UTCTime
atiCreationDate = lens _atiCreationDate (\s a -> s {_atiCreationDate = a}) . _Time

instance FromJSON ActivityTypeInfo where
  parseJSON =
    withObject
      "ActivityTypeInfo"
      ( \x ->
          ActivityTypeInfo'
            <$> (x .:? "deprecationDate")
            <*> (x .:? "description")
            <*> (x .: "activityType")
            <*> (x .: "status")
            <*> (x .: "creationDate")
      )

instance Hashable ActivityTypeInfo

instance NFData ActivityTypeInfo
