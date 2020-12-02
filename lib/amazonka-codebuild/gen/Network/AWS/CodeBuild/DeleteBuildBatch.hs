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
-- Module      : Network.AWS.CodeBuild.DeleteBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch build.
module Network.AWS.CodeBuild.DeleteBuildBatch
  ( -- * Creating a Request
    deleteBuildBatch,
    DeleteBuildBatch,

    -- * Request Lenses
    dbbId,

    -- * Destructuring the Response
    deleteBuildBatchResponse,
    DeleteBuildBatchResponse,

    -- * Response Lenses
    dbbrsBuildsNotDeleted,
    dbbrsBuildsDeleted,
    dbbrsStatusCode,
    dbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBuildBatch' smart constructor.
newtype DeleteBuildBatch = DeleteBuildBatch' {_dbbId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBuildBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbbId' - The identifier of the batch build to delete.
deleteBuildBatch ::
  -- | 'dbbId'
  Text ->
  DeleteBuildBatch
deleteBuildBatch pId_ = DeleteBuildBatch' {_dbbId = pId_}

-- | The identifier of the batch build to delete.
dbbId :: Lens' DeleteBuildBatch Text
dbbId = lens _dbbId (\s a -> s {_dbbId = a})

instance AWSRequest DeleteBuildBatch where
  type Rs DeleteBuildBatch = DeleteBuildBatchResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          DeleteBuildBatchResponse'
            <$> (x .?> "buildsNotDeleted" .!@ mempty)
            <*> (x .?> "buildsDeleted")
            <*> (x .?> "statusCode")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteBuildBatch

instance NFData DeleteBuildBatch

instance ToHeaders DeleteBuildBatch where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.DeleteBuildBatch" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteBuildBatch where
  toJSON DeleteBuildBatch' {..} =
    object (catMaybes [Just ("id" .= _dbbId)])

instance ToPath DeleteBuildBatch where
  toPath = const "/"

instance ToQuery DeleteBuildBatch where
  toQuery = const mempty

-- | /See:/ 'deleteBuildBatchResponse' smart constructor.
data DeleteBuildBatchResponse = DeleteBuildBatchResponse'
  { _dbbrsBuildsNotDeleted ::
      !(Maybe [BuildNotDeleted]),
    _dbbrsBuildsDeleted ::
      !(Maybe (List1 Text)),
    _dbbrsStatusCode :: !(Maybe Text),
    _dbbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBuildBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbbrsBuildsNotDeleted' - An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
--
-- * 'dbbrsBuildsDeleted' - An array of strings that contain the identifiers of the builds that were deleted.
--
-- * 'dbbrsStatusCode' - The status code.
--
-- * 'dbbrsResponseStatus' - -- | The response status code.
deleteBuildBatchResponse ::
  -- | 'dbbrsResponseStatus'
  Int ->
  DeleteBuildBatchResponse
deleteBuildBatchResponse pResponseStatus_ =
  DeleteBuildBatchResponse'
    { _dbbrsBuildsNotDeleted = Nothing,
      _dbbrsBuildsDeleted = Nothing,
      _dbbrsStatusCode = Nothing,
      _dbbrsResponseStatus = pResponseStatus_
    }

-- | An array of @BuildNotDeleted@ objects that specify the builds that could not be deleted.
dbbrsBuildsNotDeleted :: Lens' DeleteBuildBatchResponse [BuildNotDeleted]
dbbrsBuildsNotDeleted = lens _dbbrsBuildsNotDeleted (\s a -> s {_dbbrsBuildsNotDeleted = a}) . _Default . _Coerce

-- | An array of strings that contain the identifiers of the builds that were deleted.
dbbrsBuildsDeleted :: Lens' DeleteBuildBatchResponse (Maybe (NonEmpty Text))
dbbrsBuildsDeleted = lens _dbbrsBuildsDeleted (\s a -> s {_dbbrsBuildsDeleted = a}) . mapping _List1

-- | The status code.
dbbrsStatusCode :: Lens' DeleteBuildBatchResponse (Maybe Text)
dbbrsStatusCode = lens _dbbrsStatusCode (\s a -> s {_dbbrsStatusCode = a})

-- | -- | The response status code.
dbbrsResponseStatus :: Lens' DeleteBuildBatchResponse Int
dbbrsResponseStatus = lens _dbbrsResponseStatus (\s a -> s {_dbbrsResponseStatus = a})

instance NFData DeleteBuildBatchResponse
