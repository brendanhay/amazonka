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
-- Module      : Network.AWS.Glue.DeleteSchemaVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove versions from the specified schema. A version number or range may be supplied. If the compatibility mode forbids deleting of a version that is necessary, such as BACKWARDS_FULL, an error is returned. Calling the @GetSchemaVersions@ API after this call will list the status of the deleted versions.
--
--
-- When the range of version numbers contain check pointed version, the API will return a 409 conflict and will not proceed with the deletion. You have to remove the checkpoint first using the @DeleteSchemaCheckpoint@ API before using this API.
--
-- You cannot use the @DeleteSchemaVersions@ API to delete the first schema version in the schema set. The first schema version can only be deleted by the @DeleteSchema@ API. This operation will also delete the attached @SchemaVersionMetadata@ under the schema versions. Hard deletes will be enforced on the database.
--
-- If the compatibility mode forbids deleting of a version that is necessary, such as BACKWARDS_FULL, an error is returned.
module Network.AWS.Glue.DeleteSchemaVersions
  ( -- * Creating a Request
    deleteSchemaVersions,
    DeleteSchemaVersions,

    -- * Request Lenses
    dsvSchemaId,
    dsvVersions,

    -- * Destructuring the Response
    deleteSchemaVersionsResponse,
    DeleteSchemaVersionsResponse,

    -- * Response Lenses
    dsvrsSchemaVersionErrors,
    dsvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSchemaVersions' smart constructor.
data DeleteSchemaVersions = DeleteSchemaVersions'
  { _dsvSchemaId ::
      !SchemaId,
    _dsvVersions :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSchemaVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsvSchemaId' - This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- * 'dsvVersions' - A version range may be supplied which may be of the format:     * a single version number, 5     * a range, 5-8 : deletes versions 5, 6, 7, 8
deleteSchemaVersions ::
  -- | 'dsvSchemaId'
  SchemaId ->
  -- | 'dsvVersions'
  Text ->
  DeleteSchemaVersions
deleteSchemaVersions pSchemaId_ pVersions_ =
  DeleteSchemaVersions'
    { _dsvSchemaId = pSchemaId_,
      _dsvVersions = pVersions_
    }

-- | This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
dsvSchemaId :: Lens' DeleteSchemaVersions SchemaId
dsvSchemaId = lens _dsvSchemaId (\s a -> s {_dsvSchemaId = a})

-- | A version range may be supplied which may be of the format:     * a single version number, 5     * a range, 5-8 : deletes versions 5, 6, 7, 8
dsvVersions :: Lens' DeleteSchemaVersions Text
dsvVersions = lens _dsvVersions (\s a -> s {_dsvVersions = a})

instance AWSRequest DeleteSchemaVersions where
  type Rs DeleteSchemaVersions = DeleteSchemaVersionsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          DeleteSchemaVersionsResponse'
            <$> (x .?> "SchemaVersionErrors" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteSchemaVersions

instance NFData DeleteSchemaVersions

instance ToHeaders DeleteSchemaVersions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.DeleteSchemaVersions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteSchemaVersions where
  toJSON DeleteSchemaVersions' {..} =
    object
      ( catMaybes
          [ Just ("SchemaId" .= _dsvSchemaId),
            Just ("Versions" .= _dsvVersions)
          ]
      )

instance ToPath DeleteSchemaVersions where
  toPath = const "/"

instance ToQuery DeleteSchemaVersions where
  toQuery = const mempty

-- | /See:/ 'deleteSchemaVersionsResponse' smart constructor.
data DeleteSchemaVersionsResponse = DeleteSchemaVersionsResponse'
  { _dsvrsSchemaVersionErrors ::
      !(Maybe [SchemaVersionErrorItem]),
    _dsvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSchemaVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsvrsSchemaVersionErrors' - A list of @SchemaVersionErrorItem@ objects, each containing an error and schema version.
--
-- * 'dsvrsResponseStatus' - -- | The response status code.
deleteSchemaVersionsResponse ::
  -- | 'dsvrsResponseStatus'
  Int ->
  DeleteSchemaVersionsResponse
deleteSchemaVersionsResponse pResponseStatus_ =
  DeleteSchemaVersionsResponse'
    { _dsvrsSchemaVersionErrors =
        Nothing,
      _dsvrsResponseStatus = pResponseStatus_
    }

-- | A list of @SchemaVersionErrorItem@ objects, each containing an error and schema version.
dsvrsSchemaVersionErrors :: Lens' DeleteSchemaVersionsResponse [SchemaVersionErrorItem]
dsvrsSchemaVersionErrors = lens _dsvrsSchemaVersionErrors (\s a -> s {_dsvrsSchemaVersionErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
dsvrsResponseStatus :: Lens' DeleteSchemaVersionsResponse Int
dsvrsResponseStatus = lens _dsvrsResponseStatus (\s a -> s {_dsvrsResponseStatus = a})

instance NFData DeleteSchemaVersionsResponse
