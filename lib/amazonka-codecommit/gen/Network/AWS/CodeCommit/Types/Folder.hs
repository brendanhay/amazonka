{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Folder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Folder where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a folder in a repository.
--
--
--
-- /See:/ 'folder' smart constructor.
data Folder = Folder'
  { _folAbsolutePath :: !(Maybe Text),
    _folTreeId :: !(Maybe Text),
    _folRelativePath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Folder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'folAbsolutePath' - The fully qualified path of the folder in the repository.
--
-- * 'folTreeId' - The full SHA-1 pointer of the tree information for the commit that contains the folder.
--
-- * 'folRelativePath' - The relative path of the specified folder from the folder where the query originated.
folder ::
  Folder
folder =
  Folder'
    { _folAbsolutePath = Nothing,
      _folTreeId = Nothing,
      _folRelativePath = Nothing
    }

-- | The fully qualified path of the folder in the repository.
folAbsolutePath :: Lens' Folder (Maybe Text)
folAbsolutePath = lens _folAbsolutePath (\s a -> s {_folAbsolutePath = a})

-- | The full SHA-1 pointer of the tree information for the commit that contains the folder.
folTreeId :: Lens' Folder (Maybe Text)
folTreeId = lens _folTreeId (\s a -> s {_folTreeId = a})

-- | The relative path of the specified folder from the folder where the query originated.
folRelativePath :: Lens' Folder (Maybe Text)
folRelativePath = lens _folRelativePath (\s a -> s {_folRelativePath = a})

instance FromJSON Folder where
  parseJSON =
    withObject
      "Folder"
      ( \x ->
          Folder'
            <$> (x .:? "absolutePath")
            <*> (x .:? "treeId")
            <*> (x .:? "relativePath")
      )

instance Hashable Folder

instance NFData Folder
