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
-- Module      : Network.AWS.Glue.GetSchemaVersionsDiff
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the schema version difference in the specified difference type between two stored schema versions in the Schema Registry.
--
--
-- This API allows you to compare two schema versions between two schema definitions under the same schema.
module Network.AWS.Glue.GetSchemaVersionsDiff
  ( -- * Creating a Request
    getSchemaVersionsDiff,
    GetSchemaVersionsDiff,

    -- * Request Lenses
    gsvdSchemaId,
    gsvdFirstSchemaVersionNumber,
    gsvdSecondSchemaVersionNumber,
    gsvdSchemaDiffType,

    -- * Destructuring the Response
    getSchemaVersionsDiffResponse,
    GetSchemaVersionsDiffResponse,

    -- * Response Lenses
    gsvdrsDiff,
    gsvdrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSchemaVersionsDiff' smart constructor.
data GetSchemaVersionsDiff = GetSchemaVersionsDiff'
  { _gsvdSchemaId ::
      !SchemaId,
    _gsvdFirstSchemaVersionNumber ::
      !SchemaVersionNumber,
    _gsvdSecondSchemaVersionNumber ::
      !SchemaVersionNumber,
    _gsvdSchemaDiffType :: !SchemaDiffType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSchemaVersionsDiff' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvdSchemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
-- * 'gsvdFirstSchemaVersionNumber' - The first of the two schema versions to be compared.
--
-- * 'gsvdSecondSchemaVersionNumber' - The second of the two schema versions to be compared.
--
-- * 'gsvdSchemaDiffType' - Refers to @SYNTAX_DIFF@ , which is the currently supported diff type.
getSchemaVersionsDiff ::
  -- | 'gsvdSchemaId'
  SchemaId ->
  -- | 'gsvdFirstSchemaVersionNumber'
  SchemaVersionNumber ->
  -- | 'gsvdSecondSchemaVersionNumber'
  SchemaVersionNumber ->
  -- | 'gsvdSchemaDiffType'
  SchemaDiffType ->
  GetSchemaVersionsDiff
getSchemaVersionsDiff
  pSchemaId_
  pFirstSchemaVersionNumber_
  pSecondSchemaVersionNumber_
  pSchemaDiffType_ =
    GetSchemaVersionsDiff'
      { _gsvdSchemaId = pSchemaId_,
        _gsvdFirstSchemaVersionNumber = pFirstSchemaVersionNumber_,
        _gsvdSecondSchemaVersionNumber = pSecondSchemaVersionNumber_,
        _gsvdSchemaDiffType = pSchemaDiffType_
      }

-- | This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
gsvdSchemaId :: Lens' GetSchemaVersionsDiff SchemaId
gsvdSchemaId = lens _gsvdSchemaId (\s a -> s {_gsvdSchemaId = a})

-- | The first of the two schema versions to be compared.
gsvdFirstSchemaVersionNumber :: Lens' GetSchemaVersionsDiff SchemaVersionNumber
gsvdFirstSchemaVersionNumber = lens _gsvdFirstSchemaVersionNumber (\s a -> s {_gsvdFirstSchemaVersionNumber = a})

-- | The second of the two schema versions to be compared.
gsvdSecondSchemaVersionNumber :: Lens' GetSchemaVersionsDiff SchemaVersionNumber
gsvdSecondSchemaVersionNumber = lens _gsvdSecondSchemaVersionNumber (\s a -> s {_gsvdSecondSchemaVersionNumber = a})

-- | Refers to @SYNTAX_DIFF@ , which is the currently supported diff type.
gsvdSchemaDiffType :: Lens' GetSchemaVersionsDiff SchemaDiffType
gsvdSchemaDiffType = lens _gsvdSchemaDiffType (\s a -> s {_gsvdSchemaDiffType = a})

instance AWSRequest GetSchemaVersionsDiff where
  type Rs GetSchemaVersionsDiff = GetSchemaVersionsDiffResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetSchemaVersionsDiffResponse'
            <$> (x .?> "Diff") <*> (pure (fromEnum s))
      )

instance Hashable GetSchemaVersionsDiff

instance NFData GetSchemaVersionsDiff

instance ToHeaders GetSchemaVersionsDiff where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetSchemaVersionsDiff" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSchemaVersionsDiff where
  toJSON GetSchemaVersionsDiff' {..} =
    object
      ( catMaybes
          [ Just ("SchemaId" .= _gsvdSchemaId),
            Just ("FirstSchemaVersionNumber" .= _gsvdFirstSchemaVersionNumber),
            Just
              ("SecondSchemaVersionNumber" .= _gsvdSecondSchemaVersionNumber),
            Just ("SchemaDiffType" .= _gsvdSchemaDiffType)
          ]
      )

instance ToPath GetSchemaVersionsDiff where
  toPath = const "/"

instance ToQuery GetSchemaVersionsDiff where
  toQuery = const mempty

-- | /See:/ 'getSchemaVersionsDiffResponse' smart constructor.
data GetSchemaVersionsDiffResponse = GetSchemaVersionsDiffResponse'
  { _gsvdrsDiff ::
      !(Maybe Text),
    _gsvdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSchemaVersionsDiffResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvdrsDiff' - The difference between schemas as a string in JsonPatch format.
--
-- * 'gsvdrsResponseStatus' - -- | The response status code.
getSchemaVersionsDiffResponse ::
  -- | 'gsvdrsResponseStatus'
  Int ->
  GetSchemaVersionsDiffResponse
getSchemaVersionsDiffResponse pResponseStatus_ =
  GetSchemaVersionsDiffResponse'
    { _gsvdrsDiff = Nothing,
      _gsvdrsResponseStatus = pResponseStatus_
    }

-- | The difference between schemas as a string in JsonPatch format.
gsvdrsDiff :: Lens' GetSchemaVersionsDiffResponse (Maybe Text)
gsvdrsDiff = lens _gsvdrsDiff (\s a -> s {_gsvdrsDiff = a})

-- | -- | The response status code.
gsvdrsResponseStatus :: Lens' GetSchemaVersionsDiffResponse Int
gsvdrsResponseStatus = lens _gsvdrsResponseStatus (\s a -> s {_gsvdrsResponseStatus = a})

instance NFData GetSchemaVersionsDiffResponse
