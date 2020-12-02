{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the contents of a specified folder in a repository.
module Network.AWS.CodeCommit.GetFolder
  ( -- * Creating a Request
    getFolder,
    GetFolder,

    -- * Request Lenses
    gfCommitSpecifier,
    gfRepositoryName,
    gfFolderPath,

    -- * Destructuring the Response
    getFolderResponse,
    GetFolderResponse,

    -- * Response Lenses
    gfrsSubModules,
    gfrsTreeId,
    gfrsSubFolders,
    gfrsSymbolicLinks,
    gfrsFiles,
    gfrsResponseStatus,
    gfrsCommitId,
    gfrsFolderPath,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFolder' smart constructor.
data GetFolder = GetFolder'
  { _gfCommitSpecifier :: !(Maybe Text),
    _gfRepositoryName :: !Text,
    _gfFolderPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFolder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfCommitSpecifier' - A fully qualified reference used to identify a commit that contains the version of the folder's content to return. A fully qualified reference can be a commit ID, branch name, tag, or reference such as HEAD. If no specifier is provided, the folder content is returned as it exists in the HEAD commit.
--
-- * 'gfRepositoryName' - The name of the repository.
--
-- * 'gfFolderPath' - The fully qualified path to the folder whose contents are returned, including the folder name. For example, /examples is a fully-qualified path to a folder named examples that was created off of the root directory (/) of a repository.
getFolder ::
  -- | 'gfRepositoryName'
  Text ->
  -- | 'gfFolderPath'
  Text ->
  GetFolder
getFolder pRepositoryName_ pFolderPath_ =
  GetFolder'
    { _gfCommitSpecifier = Nothing,
      _gfRepositoryName = pRepositoryName_,
      _gfFolderPath = pFolderPath_
    }

-- | A fully qualified reference used to identify a commit that contains the version of the folder's content to return. A fully qualified reference can be a commit ID, branch name, tag, or reference such as HEAD. If no specifier is provided, the folder content is returned as it exists in the HEAD commit.
gfCommitSpecifier :: Lens' GetFolder (Maybe Text)
gfCommitSpecifier = lens _gfCommitSpecifier (\s a -> s {_gfCommitSpecifier = a})

-- | The name of the repository.
gfRepositoryName :: Lens' GetFolder Text
gfRepositoryName = lens _gfRepositoryName (\s a -> s {_gfRepositoryName = a})

-- | The fully qualified path to the folder whose contents are returned, including the folder name. For example, /examples is a fully-qualified path to a folder named examples that was created off of the root directory (/) of a repository.
gfFolderPath :: Lens' GetFolder Text
gfFolderPath = lens _gfFolderPath (\s a -> s {_gfFolderPath = a})

instance AWSRequest GetFolder where
  type Rs GetFolder = GetFolderResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          GetFolderResponse'
            <$> (x .?> "subModules" .!@ mempty)
            <*> (x .?> "treeId")
            <*> (x .?> "subFolders" .!@ mempty)
            <*> (x .?> "symbolicLinks" .!@ mempty)
            <*> (x .?> "files" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "commitId")
            <*> (x .:> "folderPath")
      )

instance Hashable GetFolder

instance NFData GetFolder

instance ToHeaders GetFolder where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("CodeCommit_20150413.GetFolder" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetFolder where
  toJSON GetFolder' {..} =
    object
      ( catMaybes
          [ ("commitSpecifier" .=) <$> _gfCommitSpecifier,
            Just ("repositoryName" .= _gfRepositoryName),
            Just ("folderPath" .= _gfFolderPath)
          ]
      )

instance ToPath GetFolder where
  toPath = const "/"

instance ToQuery GetFolder where
  toQuery = const mempty

-- | /See:/ 'getFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { _gfrsSubModules ::
      !(Maybe [SubModule]),
    _gfrsTreeId :: !(Maybe Text),
    _gfrsSubFolders :: !(Maybe [Folder]),
    _gfrsSymbolicLinks :: !(Maybe [SymbolicLink]),
    _gfrsFiles :: !(Maybe [File]),
    _gfrsResponseStatus :: !Int,
    _gfrsCommitId :: !Text,
    _gfrsFolderPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFolderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfrsSubModules' - The list of submodules in the specified folder, if any.
--
-- * 'gfrsTreeId' - The full SHA-1 pointer of the tree information for the commit that contains the folder.
--
-- * 'gfrsSubFolders' - The list of folders that exist under the specified folder, if any.
--
-- * 'gfrsSymbolicLinks' - The list of symbolic links to other files and folders in the specified folder, if any.
--
-- * 'gfrsFiles' - The list of files in the specified folder, if any.
--
-- * 'gfrsResponseStatus' - -- | The response status code.
--
-- * 'gfrsCommitId' - The full commit ID used as a reference for the returned version of the folder content.
--
-- * 'gfrsFolderPath' - The fully qualified path of the folder whose contents are returned.
getFolderResponse ::
  -- | 'gfrsResponseStatus'
  Int ->
  -- | 'gfrsCommitId'
  Text ->
  -- | 'gfrsFolderPath'
  Text ->
  GetFolderResponse
getFolderResponse pResponseStatus_ pCommitId_ pFolderPath_ =
  GetFolderResponse'
    { _gfrsSubModules = Nothing,
      _gfrsTreeId = Nothing,
      _gfrsSubFolders = Nothing,
      _gfrsSymbolicLinks = Nothing,
      _gfrsFiles = Nothing,
      _gfrsResponseStatus = pResponseStatus_,
      _gfrsCommitId = pCommitId_,
      _gfrsFolderPath = pFolderPath_
    }

-- | The list of submodules in the specified folder, if any.
gfrsSubModules :: Lens' GetFolderResponse [SubModule]
gfrsSubModules = lens _gfrsSubModules (\s a -> s {_gfrsSubModules = a}) . _Default . _Coerce

-- | The full SHA-1 pointer of the tree information for the commit that contains the folder.
gfrsTreeId :: Lens' GetFolderResponse (Maybe Text)
gfrsTreeId = lens _gfrsTreeId (\s a -> s {_gfrsTreeId = a})

-- | The list of folders that exist under the specified folder, if any.
gfrsSubFolders :: Lens' GetFolderResponse [Folder]
gfrsSubFolders = lens _gfrsSubFolders (\s a -> s {_gfrsSubFolders = a}) . _Default . _Coerce

-- | The list of symbolic links to other files and folders in the specified folder, if any.
gfrsSymbolicLinks :: Lens' GetFolderResponse [SymbolicLink]
gfrsSymbolicLinks = lens _gfrsSymbolicLinks (\s a -> s {_gfrsSymbolicLinks = a}) . _Default . _Coerce

-- | The list of files in the specified folder, if any.
gfrsFiles :: Lens' GetFolderResponse [File]
gfrsFiles = lens _gfrsFiles (\s a -> s {_gfrsFiles = a}) . _Default . _Coerce

-- | -- | The response status code.
gfrsResponseStatus :: Lens' GetFolderResponse Int
gfrsResponseStatus = lens _gfrsResponseStatus (\s a -> s {_gfrsResponseStatus = a})

-- | The full commit ID used as a reference for the returned version of the folder content.
gfrsCommitId :: Lens' GetFolderResponse Text
gfrsCommitId = lens _gfrsCommitId (\s a -> s {_gfrsCommitId = a})

-- | The fully qualified path of the folder whose contents are returned.
gfrsFolderPath :: Lens' GetFolderResponse Text
gfrsFolderPath = lens _gfrsFolderPath (\s a -> s {_gfrsFolderPath = a})

instance NFData GetFolderResponse
