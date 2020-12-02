{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Directory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Directory where

import Network.AWS.CloudDirectory.Types.DirectoryState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Directory structure that includes the directory name and directory ARN.
--
--
--
-- /See:/ 'directory' smart constructor.
data Directory = Directory'
  { _dDirectoryARN :: !(Maybe Text),
    _dState :: !(Maybe DirectoryState),
    _dName :: !(Maybe Text),
    _dCreationDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Directory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
--
-- * 'dState' - The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
--
-- * 'dName' - The name of the directory.
--
-- * 'dCreationDateTime' - The date and time when the directory was created.
directory ::
  Directory
directory =
  Directory'
    { _dDirectoryARN = Nothing,
      _dState = Nothing,
      _dName = Nothing,
      _dCreationDateTime = Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
dDirectoryARN :: Lens' Directory (Maybe Text)
dDirectoryARN = lens _dDirectoryARN (\s a -> s {_dDirectoryARN = a})

-- | The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
dState :: Lens' Directory (Maybe DirectoryState)
dState = lens _dState (\s a -> s {_dState = a})

-- | The name of the directory.
dName :: Lens' Directory (Maybe Text)
dName = lens _dName (\s a -> s {_dName = a})

-- | The date and time when the directory was created.
dCreationDateTime :: Lens' Directory (Maybe UTCTime)
dCreationDateTime = lens _dCreationDateTime (\s a -> s {_dCreationDateTime = a}) . mapping _Time

instance FromJSON Directory where
  parseJSON =
    withObject
      "Directory"
      ( \x ->
          Directory'
            <$> (x .:? "DirectoryArn")
            <*> (x .:? "State")
            <*> (x .:? "Name")
            <*> (x .:? "CreationDateTime")
      )

instance Hashable Directory

instance NFData Directory
