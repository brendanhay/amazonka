{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReplaceContentEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReplaceContentEntry where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.CodeCommit.Types.ReplacementTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a replacement content entry in the conflict of a merge or pull request operation.
--
--
--
-- /See:/ 'replaceContentEntry' smart constructor.
data ReplaceContentEntry = ReplaceContentEntry'
  { _rceFileMode ::
      !(Maybe FileModeTypeEnum),
    _rceContent :: !(Maybe Base64),
    _rceFilePath :: !Text,
    _rceReplacementType :: !ReplacementTypeEnum
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplaceContentEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rceFileMode' - The file mode to apply during conflict resoltion.
--
-- * 'rceContent' - The base-64 encoded content to use when the replacement type is USE_NEW_CONTENT.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'rceFilePath' - The path of the conflicting file.
--
-- * 'rceReplacementType' - The replacement type to use when determining how to resolve the conflict.
replaceContentEntry ::
  -- | 'rceFilePath'
  Text ->
  -- | 'rceReplacementType'
  ReplacementTypeEnum ->
  ReplaceContentEntry
replaceContentEntry pFilePath_ pReplacementType_ =
  ReplaceContentEntry'
    { _rceFileMode = Nothing,
      _rceContent = Nothing,
      _rceFilePath = pFilePath_,
      _rceReplacementType = pReplacementType_
    }

-- | The file mode to apply during conflict resoltion.
rceFileMode :: Lens' ReplaceContentEntry (Maybe FileModeTypeEnum)
rceFileMode = lens _rceFileMode (\s a -> s {_rceFileMode = a})

-- | The base-64 encoded content to use when the replacement type is USE_NEW_CONTENT.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rceContent :: Lens' ReplaceContentEntry (Maybe ByteString)
rceContent = lens _rceContent (\s a -> s {_rceContent = a}) . mapping _Base64

-- | The path of the conflicting file.
rceFilePath :: Lens' ReplaceContentEntry Text
rceFilePath = lens _rceFilePath (\s a -> s {_rceFilePath = a})

-- | The replacement type to use when determining how to resolve the conflict.
rceReplacementType :: Lens' ReplaceContentEntry ReplacementTypeEnum
rceReplacementType = lens _rceReplacementType (\s a -> s {_rceReplacementType = a})

instance Hashable ReplaceContentEntry

instance NFData ReplaceContentEntry

instance ToJSON ReplaceContentEntry where
  toJSON ReplaceContentEntry' {..} =
    object
      ( catMaybes
          [ ("fileMode" .=) <$> _rceFileMode,
            ("content" .=) <$> _rceContent,
            Just ("filePath" .= _rceFilePath),
            Just ("replacementType" .= _rceReplacementType)
          ]
      )
