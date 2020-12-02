{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeMetadata where

import Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a merge or potential merge between a source reference and a destination reference in a pull request.
--
--
--
-- /See:/ 'mergeMetadata' smart constructor.
data MergeMetadata = MergeMetadata'
  { _mmMergedBy :: !(Maybe Text),
    _mmMergeOption :: !(Maybe MergeOptionTypeEnum),
    _mmIsMerged :: !(Maybe Bool),
    _mmMergeCommitId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MergeMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mmMergedBy' - The Amazon Resource Name (ARN) of the user who merged the branches.
--
-- * 'mmMergeOption' - The merge strategy used in the merge.
--
-- * 'mmIsMerged' - A Boolean value indicating whether the merge has been made.
--
-- * 'mmMergeCommitId' - The commit ID for the merge commit, if any.
mergeMetadata ::
  MergeMetadata
mergeMetadata =
  MergeMetadata'
    { _mmMergedBy = Nothing,
      _mmMergeOption = Nothing,
      _mmIsMerged = Nothing,
      _mmMergeCommitId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the user who merged the branches.
mmMergedBy :: Lens' MergeMetadata (Maybe Text)
mmMergedBy = lens _mmMergedBy (\s a -> s {_mmMergedBy = a})

-- | The merge strategy used in the merge.
mmMergeOption :: Lens' MergeMetadata (Maybe MergeOptionTypeEnum)
mmMergeOption = lens _mmMergeOption (\s a -> s {_mmMergeOption = a})

-- | A Boolean value indicating whether the merge has been made.
mmIsMerged :: Lens' MergeMetadata (Maybe Bool)
mmIsMerged = lens _mmIsMerged (\s a -> s {_mmIsMerged = a})

-- | The commit ID for the merge commit, if any.
mmMergeCommitId :: Lens' MergeMetadata (Maybe Text)
mmMergeCommitId = lens _mmMergeCommitId (\s a -> s {_mmMergeCommitId = a})

instance FromJSON MergeMetadata where
  parseJSON =
    withObject
      "MergeMetadata"
      ( \x ->
          MergeMetadata'
            <$> (x .:? "mergedBy")
            <*> (x .:? "mergeOption")
            <*> (x .:? "isMerged")
            <*> (x .:? "mergeCommitId")
      )

instance Hashable MergeMetadata

instance NFData MergeMetadata
