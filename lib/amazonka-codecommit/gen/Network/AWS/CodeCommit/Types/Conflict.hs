{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Conflict
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Conflict where

import Network.AWS.CodeCommit.Types.ConflictMetadata
import Network.AWS.CodeCommit.Types.MergeHunk
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about conflicts in a merge operation.
--
--
--
-- /See:/ 'conflict' smart constructor.
data Conflict = Conflict'
  { _cMergeHunks :: !(Maybe [MergeHunk]),
    _cConflictMetadata :: !(Maybe ConflictMetadata)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Conflict' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cMergeHunks' - A list of hunks that contain the differences between files or lines causing the conflict.
--
-- * 'cConflictMetadata' - Metadata about a conflict in a merge operation.
conflict ::
  Conflict
conflict =
  Conflict' {_cMergeHunks = Nothing, _cConflictMetadata = Nothing}

-- | A list of hunks that contain the differences between files or lines causing the conflict.
cMergeHunks :: Lens' Conflict [MergeHunk]
cMergeHunks = lens _cMergeHunks (\s a -> s {_cMergeHunks = a}) . _Default . _Coerce

-- | Metadata about a conflict in a merge operation.
cConflictMetadata :: Lens' Conflict (Maybe ConflictMetadata)
cConflictMetadata = lens _cConflictMetadata (\s a -> s {_cConflictMetadata = a})

instance FromJSON Conflict where
  parseJSON =
    withObject
      "Conflict"
      ( \x ->
          Conflict'
            <$> (x .:? "mergeHunks" .!= mempty) <*> (x .:? "conflictMetadata")
      )

instance Hashable Conflict

instance NFData Conflict
