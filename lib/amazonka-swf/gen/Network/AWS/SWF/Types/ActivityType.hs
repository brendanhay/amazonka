{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an activity type.
--
--
--
-- /See:/ 'activityType' smart constructor.
data ActivityType = ActivityType'
  { _atName :: !Text,
    _atVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atName' - The name of this activity.
--
-- * 'atVersion' - The version of this activity.
activityType ::
  -- | 'atName'
  Text ->
  -- | 'atVersion'
  Text ->
  ActivityType
activityType pName_ pVersion_ =
  ActivityType' {_atName = pName_, _atVersion = pVersion_}

-- | The name of this activity.
atName :: Lens' ActivityType Text
atName = lens _atName (\s a -> s {_atName = a})

-- | The version of this activity.
atVersion :: Lens' ActivityType Text
atVersion = lens _atVersion (\s a -> s {_atVersion = a})

instance FromJSON ActivityType where
  parseJSON =
    withObject
      "ActivityType"
      (\x -> ActivityType' <$> (x .: "name") <*> (x .: "version"))

instance Hashable ActivityType

instance NFData ActivityType

instance ToJSON ActivityType where
  toJSON ActivityType' {..} =
    object
      ( catMaybes
          [Just ("name" .= _atName), Just ("version" .= _atVersion)]
      )
