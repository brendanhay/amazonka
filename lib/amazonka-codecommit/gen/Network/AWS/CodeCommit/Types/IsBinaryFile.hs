{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.IsBinaryFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.IsBinaryFile where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about whether a file is binary or textual in a merge or pull request operation.
--
--
--
-- /See:/ 'isBinaryFile' smart constructor.
data IsBinaryFile = IsBinaryFile'
  { _ibfDestination :: !(Maybe Bool),
    _ibfBase :: !(Maybe Bool),
    _ibfSource :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IsBinaryFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibfDestination' - The binary or non-binary status of a file in the destination of a merge or pull request.
--
-- * 'ibfBase' - The binary or non-binary status of a file in the base of a merge or pull request.
--
-- * 'ibfSource' - The binary or non-binary status of file in the source of a merge or pull request.
isBinaryFile ::
  IsBinaryFile
isBinaryFile =
  IsBinaryFile'
    { _ibfDestination = Nothing,
      _ibfBase = Nothing,
      _ibfSource = Nothing
    }

-- | The binary or non-binary status of a file in the destination of a merge or pull request.
ibfDestination :: Lens' IsBinaryFile (Maybe Bool)
ibfDestination = lens _ibfDestination (\s a -> s {_ibfDestination = a})

-- | The binary or non-binary status of a file in the base of a merge or pull request.
ibfBase :: Lens' IsBinaryFile (Maybe Bool)
ibfBase = lens _ibfBase (\s a -> s {_ibfBase = a})

-- | The binary or non-binary status of file in the source of a merge or pull request.
ibfSource :: Lens' IsBinaryFile (Maybe Bool)
ibfSource = lens _ibfSource (\s a -> s {_ibfSource = a})

instance FromJSON IsBinaryFile where
  parseJSON =
    withObject
      "IsBinaryFile"
      ( \x ->
          IsBinaryFile'
            <$> (x .:? "destination") <*> (x .:? "base") <*> (x .:? "source")
      )

instance Hashable IsBinaryFile

instance NFData IsBinaryFile
