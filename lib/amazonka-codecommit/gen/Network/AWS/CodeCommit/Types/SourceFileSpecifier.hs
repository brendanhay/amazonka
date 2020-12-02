{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SourceFileSpecifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SourceFileSpecifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a source file that is part of changes made in a commit.
--
--
--
-- /See:/ 'sourceFileSpecifier' smart constructor.
data SourceFileSpecifier = SourceFileSpecifier'
  { _sfsIsMove ::
      !(Maybe Bool),
    _sfsFilePath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceFileSpecifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfsIsMove' - Whether to remove the source file from the parent commit.
--
-- * 'sfsFilePath' - The full path to the file, including the name of the file.
sourceFileSpecifier ::
  -- | 'sfsFilePath'
  Text ->
  SourceFileSpecifier
sourceFileSpecifier pFilePath_ =
  SourceFileSpecifier'
    { _sfsIsMove = Nothing,
      _sfsFilePath = pFilePath_
    }

-- | Whether to remove the source file from the parent commit.
sfsIsMove :: Lens' SourceFileSpecifier (Maybe Bool)
sfsIsMove = lens _sfsIsMove (\s a -> s {_sfsIsMove = a})

-- | The full path to the file, including the name of the file.
sfsFilePath :: Lens' SourceFileSpecifier Text
sfsFilePath = lens _sfsFilePath (\s a -> s {_sfsFilePath = a})

instance Hashable SourceFileSpecifier

instance NFData SourceFileSpecifier

instance ToJSON SourceFileSpecifier where
  toJSON SourceFileSpecifier' {..} =
    object
      ( catMaybes
          [("isMove" .=) <$> _sfsIsMove, Just ("filePath" .= _sfsFilePath)]
      )
