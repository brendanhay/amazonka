{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.DeleteFileEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.DeleteFileEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A file that is deleted as part of a commit.
--
--
--
-- /See:/ 'deleteFileEntry' smart constructor.
newtype DeleteFileEntry = DeleteFileEntry' {_dfeFilePath :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFileEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfeFilePath' - The full path of the file to be deleted, including the name of the file.
deleteFileEntry ::
  -- | 'dfeFilePath'
  Text ->
  DeleteFileEntry
deleteFileEntry pFilePath_ =
  DeleteFileEntry' {_dfeFilePath = pFilePath_}

-- | The full path of the file to be deleted, including the name of the file.
dfeFilePath :: Lens' DeleteFileEntry Text
dfeFilePath = lens _dfeFilePath (\s a -> s {_dfeFilePath = a})

instance Hashable DeleteFileEntry

instance NFData DeleteFileEntry

instance ToJSON DeleteFileEntry where
  toJSON DeleteFileEntry' {..} =
    object (catMaybes [Just ("filePath" .= _dfeFilePath)])
