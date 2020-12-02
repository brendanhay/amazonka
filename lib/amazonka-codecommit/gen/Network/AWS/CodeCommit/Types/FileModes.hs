{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileModes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileModes where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about file modes in a merge or pull request.
--
--
--
-- /See:/ 'fileModes' smart constructor.
data FileModes = FileModes'
  { _fmDestination ::
      !(Maybe FileModeTypeEnum),
    _fmBase :: !(Maybe FileModeTypeEnum),
    _fmSource :: !(Maybe FileModeTypeEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileModes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmDestination' - The file mode of a file in the destination of a merge or pull request.
--
-- * 'fmBase' - The file mode of a file in the base of a merge or pull request.
--
-- * 'fmSource' - The file mode of a file in the source of a merge or pull request.
fileModes ::
  FileModes
fileModes =
  FileModes'
    { _fmDestination = Nothing,
      _fmBase = Nothing,
      _fmSource = Nothing
    }

-- | The file mode of a file in the destination of a merge or pull request.
fmDestination :: Lens' FileModes (Maybe FileModeTypeEnum)
fmDestination = lens _fmDestination (\s a -> s {_fmDestination = a})

-- | The file mode of a file in the base of a merge or pull request.
fmBase :: Lens' FileModes (Maybe FileModeTypeEnum)
fmBase = lens _fmBase (\s a -> s {_fmBase = a})

-- | The file mode of a file in the source of a merge or pull request.
fmSource :: Lens' FileModes (Maybe FileModeTypeEnum)
fmSource = lens _fmSource (\s a -> s {_fmSource = a})

instance FromJSON FileModes where
  parseJSON =
    withObject
      "FileModes"
      ( \x ->
          FileModes'
            <$> (x .:? "destination") <*> (x .:? "base") <*> (x .:? "source")
      )

instance Hashable FileModes

instance NFData FileModes
