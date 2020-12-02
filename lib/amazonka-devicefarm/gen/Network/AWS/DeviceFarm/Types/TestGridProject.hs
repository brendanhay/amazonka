{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridProject where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A Selenium testing project. Projects are used to collect and collate sessions.
--
--
--
-- /See:/ 'testGridProject' smart constructor.
data TestGridProject = TestGridProject'
  { _tgpArn :: !(Maybe Text),
    _tgpCreated :: !(Maybe POSIX),
    _tgpName :: !(Maybe Text),
    _tgpDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestGridProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgpArn' - The ARN for the project.
--
-- * 'tgpCreated' - When the project was created.
--
-- * 'tgpName' - A human-readable name for the project.
--
-- * 'tgpDescription' - A human-readable description for the project.
testGridProject ::
  TestGridProject
testGridProject =
  TestGridProject'
    { _tgpArn = Nothing,
      _tgpCreated = Nothing,
      _tgpName = Nothing,
      _tgpDescription = Nothing
    }

-- | The ARN for the project.
tgpArn :: Lens' TestGridProject (Maybe Text)
tgpArn = lens _tgpArn (\s a -> s {_tgpArn = a})

-- | When the project was created.
tgpCreated :: Lens' TestGridProject (Maybe UTCTime)
tgpCreated = lens _tgpCreated (\s a -> s {_tgpCreated = a}) . mapping _Time

-- | A human-readable name for the project.
tgpName :: Lens' TestGridProject (Maybe Text)
tgpName = lens _tgpName (\s a -> s {_tgpName = a})

-- | A human-readable description for the project.
tgpDescription :: Lens' TestGridProject (Maybe Text)
tgpDescription = lens _tgpDescription (\s a -> s {_tgpDescription = a})

instance FromJSON TestGridProject where
  parseJSON =
    withObject
      "TestGridProject"
      ( \x ->
          TestGridProject'
            <$> (x .:? "arn")
            <*> (x .:? "created")
            <*> (x .:? "name")
            <*> (x .:? "description")
      )

instance Hashable TestGridProject

instance NFData TestGridProject
