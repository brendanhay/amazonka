{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictResolution where

import Network.AWS.CodeCommit.Types.DeleteFileEntry
import Network.AWS.CodeCommit.Types.ReplaceContentEntry
import Network.AWS.CodeCommit.Types.SetFileModeEntry
import Network.AWS.Lens
import Network.AWS.Prelude

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
--
--
-- /See:/ 'conflictResolution' smart constructor.
data ConflictResolution = ConflictResolution'
  { _crSetFileModes ::
      !(Maybe [SetFileModeEntry]),
    _crDeleteFiles :: !(Maybe [DeleteFileEntry]),
    _crReplaceContents :: !(Maybe [ReplaceContentEntry])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConflictResolution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crSetFileModes' - File modes that are set as part of the merge conflict resolution.
--
-- * 'crDeleteFiles' - Files to be deleted as part of the merge conflict resolution.
--
-- * 'crReplaceContents' - Files to have content replaced as part of the merge conflict resolution.
conflictResolution ::
  ConflictResolution
conflictResolution =
  ConflictResolution'
    { _crSetFileModes = Nothing,
      _crDeleteFiles = Nothing,
      _crReplaceContents = Nothing
    }

-- | File modes that are set as part of the merge conflict resolution.
crSetFileModes :: Lens' ConflictResolution [SetFileModeEntry]
crSetFileModes = lens _crSetFileModes (\s a -> s {_crSetFileModes = a}) . _Default . _Coerce

-- | Files to be deleted as part of the merge conflict resolution.
crDeleteFiles :: Lens' ConflictResolution [DeleteFileEntry]
crDeleteFiles = lens _crDeleteFiles (\s a -> s {_crDeleteFiles = a}) . _Default . _Coerce

-- | Files to have content replaced as part of the merge conflict resolution.
crReplaceContents :: Lens' ConflictResolution [ReplaceContentEntry]
crReplaceContents = lens _crReplaceContents (\s a -> s {_crReplaceContents = a}) . _Default . _Coerce

instance Hashable ConflictResolution

instance NFData ConflictResolution

instance ToJSON ConflictResolution where
  toJSON ConflictResolution' {..} =
    object
      ( catMaybes
          [ ("setFileModes" .=) <$> _crSetFileModes,
            ("deleteFiles" .=) <$> _crDeleteFiles,
            ("replaceContents" .=) <$> _crReplaceContents
          ]
      )
