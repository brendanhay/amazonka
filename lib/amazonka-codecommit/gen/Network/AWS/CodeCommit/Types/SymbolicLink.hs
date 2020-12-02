{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SymbolicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SymbolicLink where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a symbolic link in a repository folder.
--
--
--
-- /See:/ 'symbolicLink' smart constructor.
data SymbolicLink = SymbolicLink'
  { _slAbsolutePath :: !(Maybe Text),
    _slFileMode :: !(Maybe FileModeTypeEnum),
    _slBlobId :: !(Maybe Text),
    _slRelativePath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SymbolicLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slAbsolutePath' - The fully qualified path to the folder that contains the symbolic link.
--
-- * 'slFileMode' - The file mode permissions of the blob that cotains information about the symbolic link.
--
-- * 'slBlobId' - The blob ID that contains the information about the symbolic link.
--
-- * 'slRelativePath' - The relative path of the symbolic link from the folder where the query originated.
symbolicLink ::
  SymbolicLink
symbolicLink =
  SymbolicLink'
    { _slAbsolutePath = Nothing,
      _slFileMode = Nothing,
      _slBlobId = Nothing,
      _slRelativePath = Nothing
    }

-- | The fully qualified path to the folder that contains the symbolic link.
slAbsolutePath :: Lens' SymbolicLink (Maybe Text)
slAbsolutePath = lens _slAbsolutePath (\s a -> s {_slAbsolutePath = a})

-- | The file mode permissions of the blob that cotains information about the symbolic link.
slFileMode :: Lens' SymbolicLink (Maybe FileModeTypeEnum)
slFileMode = lens _slFileMode (\s a -> s {_slFileMode = a})

-- | The blob ID that contains the information about the symbolic link.
slBlobId :: Lens' SymbolicLink (Maybe Text)
slBlobId = lens _slBlobId (\s a -> s {_slBlobId = a})

-- | The relative path of the symbolic link from the folder where the query originated.
slRelativePath :: Lens' SymbolicLink (Maybe Text)
slRelativePath = lens _slRelativePath (\s a -> s {_slRelativePath = a})

instance FromJSON SymbolicLink where
  parseJSON =
    withObject
      "SymbolicLink"
      ( \x ->
          SymbolicLink'
            <$> (x .:? "absolutePath")
            <*> (x .:? "fileMode")
            <*> (x .:? "blobId")
            <*> (x .:? "relativePath")
      )

instance Hashable SymbolicLink

instance NFData SymbolicLink
