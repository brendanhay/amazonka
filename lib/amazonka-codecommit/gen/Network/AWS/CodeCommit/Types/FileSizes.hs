{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileSizes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileSizes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the size of files in a merge or pull request.
--
--
--
-- /See:/ 'fileSizes' smart constructor.
data FileSizes = FileSizes'
  { _fsDestination :: !(Maybe Integer),
    _fsBase :: !(Maybe Integer),
    _fsSource :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileSizes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsDestination' - The size of a file in the destination of a merge or pull request.
--
-- * 'fsBase' - The size of a file in the base of a merge or pull request.
--
-- * 'fsSource' - The size of a file in the source of a merge or pull request.
fileSizes ::
  FileSizes
fileSizes =
  FileSizes'
    { _fsDestination = Nothing,
      _fsBase = Nothing,
      _fsSource = Nothing
    }

-- | The size of a file in the destination of a merge or pull request.
fsDestination :: Lens' FileSizes (Maybe Integer)
fsDestination = lens _fsDestination (\s a -> s {_fsDestination = a})

-- | The size of a file in the base of a merge or pull request.
fsBase :: Lens' FileSizes (Maybe Integer)
fsBase = lens _fsBase (\s a -> s {_fsBase = a})

-- | The size of a file in the source of a merge or pull request.
fsSource :: Lens' FileSizes (Maybe Integer)
fsSource = lens _fsSource (\s a -> s {_fsSource = a})

instance FromJSON FileSizes where
  parseJSON =
    withObject
      "FileSizes"
      ( \x ->
          FileSizes'
            <$> (x .:? "destination") <*> (x .:? "base") <*> (x .:? "source")
      )

instance Hashable FileSizes

instance NFData FileSizes
