{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SetFileModeEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SetFileModeEntry where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the file mode changes.
--
--
--
-- /See:/ 'setFileModeEntry' smart constructor.
data SetFileModeEntry = SetFileModeEntry'
  { _sfmeFilePath :: !Text,
    _sfmeFileMode :: !FileModeTypeEnum
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetFileModeEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfmeFilePath' - The full path to the file, including the name of the file.
--
-- * 'sfmeFileMode' - The file mode for the file.
setFileModeEntry ::
  -- | 'sfmeFilePath'
  Text ->
  -- | 'sfmeFileMode'
  FileModeTypeEnum ->
  SetFileModeEntry
setFileModeEntry pFilePath_ pFileMode_ =
  SetFileModeEntry'
    { _sfmeFilePath = pFilePath_,
      _sfmeFileMode = pFileMode_
    }

-- | The full path to the file, including the name of the file.
sfmeFilePath :: Lens' SetFileModeEntry Text
sfmeFilePath = lens _sfmeFilePath (\s a -> s {_sfmeFilePath = a})

-- | The file mode for the file.
sfmeFileMode :: Lens' SetFileModeEntry FileModeTypeEnum
sfmeFileMode = lens _sfmeFileMode (\s a -> s {_sfmeFileMode = a})

instance Hashable SetFileModeEntry

instance NFData SetFileModeEntry

instance ToJSON SetFileModeEntry where
  toJSON SetFileModeEntry' {..} =
    object
      ( catMaybes
          [ Just ("filePath" .= _sfmeFilePath),
            Just ("fileMode" .= _sfmeFileMode)
          ]
      )
