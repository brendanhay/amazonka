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
-- Module      : Network.AWS.Glue.DeleteSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the entire schema set, including the schema set and all of its versions. To get the status of the delete operation, you can call @GetSchema@ API after the asynchronous call. Deleting a registry will disable all online operations for the schema, such as the @GetSchemaByDefinition@ , and @RegisterSchemaVersion@ APIs.
module Network.AWS.Glue.DeleteSchema
  ( -- * Creating a Request
    deleteSchema,
    DeleteSchema,

    -- * Request Lenses
    dsSchemaId,

    -- * Destructuring the Response
    deleteSchemaResponse,
    DeleteSchemaResponse,

    -- * Response Lenses
    dsrsStatus,
    dsrsSchemaName,
    dsrsSchemaARN,
    dsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSchema' smart constructor.
newtype DeleteSchema = DeleteSchema' {_dsSchemaId :: SchemaId}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSchemaId' - This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
deleteSchema ::
  -- | 'dsSchemaId'
  SchemaId ->
  DeleteSchema
deleteSchema pSchemaId_ = DeleteSchema' {_dsSchemaId = pSchemaId_}

-- | This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
dsSchemaId :: Lens' DeleteSchema SchemaId
dsSchemaId = lens _dsSchemaId (\s a -> s {_dsSchemaId = a})

instance AWSRequest DeleteSchema where
  type Rs DeleteSchema = DeleteSchemaResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          DeleteSchemaResponse'
            <$> (x .?> "Status")
            <*> (x .?> "SchemaName")
            <*> (x .?> "SchemaArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteSchema

instance NFData DeleteSchema

instance ToHeaders DeleteSchema where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.DeleteSchema" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteSchema where
  toJSON DeleteSchema' {..} =
    object (catMaybes [Just ("SchemaId" .= _dsSchemaId)])

instance ToPath DeleteSchema where
  toPath = const "/"

instance ToQuery DeleteSchema where
  toQuery = const mempty

-- | /See:/ 'deleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  { _dsrsStatus ::
      !(Maybe SchemaStatus),
    _dsrsSchemaName :: !(Maybe Text),
    _dsrsSchemaARN :: !(Maybe Text),
    _dsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsStatus' - The status of the schema.
--
-- * 'dsrsSchemaName' - The name of the schema being deleted.
--
-- * 'dsrsSchemaARN' - The Amazon Resource Name (ARN) of the schema being deleted.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteSchemaResponse ::
  -- | 'dsrsResponseStatus'
  Int ->
  DeleteSchemaResponse
deleteSchemaResponse pResponseStatus_ =
  DeleteSchemaResponse'
    { _dsrsStatus = Nothing,
      _dsrsSchemaName = Nothing,
      _dsrsSchemaARN = Nothing,
      _dsrsResponseStatus = pResponseStatus_
    }

-- | The status of the schema.
dsrsStatus :: Lens' DeleteSchemaResponse (Maybe SchemaStatus)
dsrsStatus = lens _dsrsStatus (\s a -> s {_dsrsStatus = a})

-- | The name of the schema being deleted.
dsrsSchemaName :: Lens' DeleteSchemaResponse (Maybe Text)
dsrsSchemaName = lens _dsrsSchemaName (\s a -> s {_dsrsSchemaName = a})

-- | The Amazon Resource Name (ARN) of the schema being deleted.
dsrsSchemaARN :: Lens' DeleteSchemaResponse (Maybe Text)
dsrsSchemaARN = lens _dsrsSchemaARN (\s a -> s {_dsrsSchemaARN = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteSchemaResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\s a -> s {_dsrsResponseStatus = a})

instance NFData DeleteSchemaResponse
