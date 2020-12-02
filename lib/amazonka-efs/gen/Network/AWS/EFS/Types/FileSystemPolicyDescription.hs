{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.FileSystemPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.FileSystemPolicyDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'fileSystemPolicyDescription' smart constructor.
data FileSystemPolicyDescription = FileSystemPolicyDescription'
  { _fspdFileSystemId ::
      !(Maybe Text),
    _fspdPolicy :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileSystemPolicyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fspdFileSystemId' - Specifies the EFS file system to which the @FileSystemPolicy@ applies.
--
-- * 'fspdPolicy' - The JSON formatted @FileSystemPolicy@ for the EFS file system.
fileSystemPolicyDescription ::
  FileSystemPolicyDescription
fileSystemPolicyDescription =
  FileSystemPolicyDescription'
    { _fspdFileSystemId = Nothing,
      _fspdPolicy = Nothing
    }

-- | Specifies the EFS file system to which the @FileSystemPolicy@ applies.
fspdFileSystemId :: Lens' FileSystemPolicyDescription (Maybe Text)
fspdFileSystemId = lens _fspdFileSystemId (\s a -> s {_fspdFileSystemId = a})

-- | The JSON formatted @FileSystemPolicy@ for the EFS file system.
fspdPolicy :: Lens' FileSystemPolicyDescription (Maybe Text)
fspdPolicy = lens _fspdPolicy (\s a -> s {_fspdPolicy = a})

instance FromJSON FileSystemPolicyDescription where
  parseJSON =
    withObject
      "FileSystemPolicyDescription"
      ( \x ->
          FileSystemPolicyDescription'
            <$> (x .:? "FileSystemId") <*> (x .:? "Policy")
      )

instance Hashable FileSystemPolicyDescription

instance NFData FileSystemPolicyDescription
