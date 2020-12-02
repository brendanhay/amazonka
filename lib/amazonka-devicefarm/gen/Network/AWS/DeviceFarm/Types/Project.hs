{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Project
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Project where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an operating-system neutral workspace for running and managing tests.
--
--
--
-- /See:/ 'project' smart constructor.
data Project = Project'
  { _pArn :: !(Maybe Text),
    _pCreated :: !(Maybe POSIX),
    _pName :: !(Maybe Text),
    _pDefaultJobTimeoutMinutes :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Project' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pArn' - The project's ARN.
--
-- * 'pCreated' - When the project was created.
--
-- * 'pName' - The project's name.
--
-- * 'pDefaultJobTimeoutMinutes' - The default number of minutes (at the project level) a test run executes before it times out. The default value is 150 minutes.
project ::
  Project
project =
  Project'
    { _pArn = Nothing,
      _pCreated = Nothing,
      _pName = Nothing,
      _pDefaultJobTimeoutMinutes = Nothing
    }

-- | The project's ARN.
pArn :: Lens' Project (Maybe Text)
pArn = lens _pArn (\s a -> s {_pArn = a})

-- | When the project was created.
pCreated :: Lens' Project (Maybe UTCTime)
pCreated = lens _pCreated (\s a -> s {_pCreated = a}) . mapping _Time

-- | The project's name.
pName :: Lens' Project (Maybe Text)
pName = lens _pName (\s a -> s {_pName = a})

-- | The default number of minutes (at the project level) a test run executes before it times out. The default value is 150 minutes.
pDefaultJobTimeoutMinutes :: Lens' Project (Maybe Int)
pDefaultJobTimeoutMinutes = lens _pDefaultJobTimeoutMinutes (\s a -> s {_pDefaultJobTimeoutMinutes = a})

instance FromJSON Project where
  parseJSON =
    withObject
      "Project"
      ( \x ->
          Project'
            <$> (x .:? "arn")
            <*> (x .:? "created")
            <*> (x .:? "name")
            <*> (x .:? "defaultJobTimeoutMinutes")
      )

instance Hashable Project

instance NFData Project
