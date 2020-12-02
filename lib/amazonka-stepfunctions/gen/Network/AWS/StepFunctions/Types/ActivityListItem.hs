{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityListItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an activity.
--
--
--
-- /See:/ 'activityListItem' smart constructor.
data ActivityListItem = ActivityListItem'
  { _aliActivityARN :: !Text,
    _aliName :: !Text,
    _aliCreationDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aliActivityARN' - The Amazon Resource Name (ARN) that identifies the activity.
--
-- * 'aliName' - The name of the activity. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- * 'aliCreationDate' - The date the activity is created.
activityListItem ::
  -- | 'aliActivityARN'
  Text ->
  -- | 'aliName'
  Text ->
  -- | 'aliCreationDate'
  UTCTime ->
  ActivityListItem
activityListItem pActivityARN_ pName_ pCreationDate_ =
  ActivityListItem'
    { _aliActivityARN = pActivityARN_,
      _aliName = pName_,
      _aliCreationDate = _Time # pCreationDate_
    }

-- | The Amazon Resource Name (ARN) that identifies the activity.
aliActivityARN :: Lens' ActivityListItem Text
aliActivityARN = lens _aliActivityARN (\s a -> s {_aliActivityARN = a})

-- | The name of the activity. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
aliName :: Lens' ActivityListItem Text
aliName = lens _aliName (\s a -> s {_aliName = a})

-- | The date the activity is created.
aliCreationDate :: Lens' ActivityListItem UTCTime
aliCreationDate = lens _aliCreationDate (\s a -> s {_aliCreationDate = a}) . _Time

instance FromJSON ActivityListItem where
  parseJSON =
    withObject
      "ActivityListItem"
      ( \x ->
          ActivityListItem'
            <$> (x .: "activityArn") <*> (x .: "name") <*> (x .: "creationDate")
      )

instance Hashable ActivityListItem

instance NFData ActivityListItem
