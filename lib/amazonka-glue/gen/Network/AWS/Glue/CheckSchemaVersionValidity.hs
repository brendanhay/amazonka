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
-- Module      : Network.AWS.Glue.CheckSchemaVersionValidity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the supplied schema. This call has no side effects, it simply validates using the supplied schema using @DataFormat@ as the format. Since it does not take a schema set name, no compatibility checks are performed.
module Network.AWS.Glue.CheckSchemaVersionValidity
  ( -- * Creating a Request
    checkSchemaVersionValidity,
    CheckSchemaVersionValidity,

    -- * Request Lenses
    csvvDataFormat,
    csvvSchemaDefinition,

    -- * Destructuring the Response
    checkSchemaVersionValidityResponse,
    CheckSchemaVersionValidityResponse,

    -- * Response Lenses
    csvvrsError,
    csvvrsValid,
    csvvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'checkSchemaVersionValidity' smart constructor.
data CheckSchemaVersionValidity = CheckSchemaVersionValidity'
  { _csvvDataFormat ::
      !DataFormat,
    _csvvSchemaDefinition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CheckSchemaVersionValidity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csvvDataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- * 'csvvSchemaDefinition' - The definition of the schema that has to be validated.
checkSchemaVersionValidity ::
  -- | 'csvvDataFormat'
  DataFormat ->
  -- | 'csvvSchemaDefinition'
  Text ->
  CheckSchemaVersionValidity
checkSchemaVersionValidity pDataFormat_ pSchemaDefinition_ =
  CheckSchemaVersionValidity'
    { _csvvDataFormat = pDataFormat_,
      _csvvSchemaDefinition = pSchemaDefinition_
    }

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
csvvDataFormat :: Lens' CheckSchemaVersionValidity DataFormat
csvvDataFormat = lens _csvvDataFormat (\s a -> s {_csvvDataFormat = a})

-- | The definition of the schema that has to be validated.
csvvSchemaDefinition :: Lens' CheckSchemaVersionValidity Text
csvvSchemaDefinition = lens _csvvSchemaDefinition (\s a -> s {_csvvSchemaDefinition = a})

instance AWSRequest CheckSchemaVersionValidity where
  type
    Rs CheckSchemaVersionValidity =
      CheckSchemaVersionValidityResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          CheckSchemaVersionValidityResponse'
            <$> (x .?> "Error") <*> (x .?> "Valid") <*> (pure (fromEnum s))
      )

instance Hashable CheckSchemaVersionValidity

instance NFData CheckSchemaVersionValidity

instance ToHeaders CheckSchemaVersionValidity where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.CheckSchemaVersionValidity" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CheckSchemaVersionValidity where
  toJSON CheckSchemaVersionValidity' {..} =
    object
      ( catMaybes
          [ Just ("DataFormat" .= _csvvDataFormat),
            Just ("SchemaDefinition" .= _csvvSchemaDefinition)
          ]
      )

instance ToPath CheckSchemaVersionValidity where
  toPath = const "/"

instance ToQuery CheckSchemaVersionValidity where
  toQuery = const mempty

-- | /See:/ 'checkSchemaVersionValidityResponse' smart constructor.
data CheckSchemaVersionValidityResponse = CheckSchemaVersionValidityResponse'
  { _csvvrsError ::
      !(Maybe Text),
    _csvvrsValid ::
      !(Maybe Bool),
    _csvvrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CheckSchemaVersionValidityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csvvrsError' - A validation failure error message.
--
-- * 'csvvrsValid' - Return true, if the schema is valid and false otherwise.
--
-- * 'csvvrsResponseStatus' - -- | The response status code.
checkSchemaVersionValidityResponse ::
  -- | 'csvvrsResponseStatus'
  Int ->
  CheckSchemaVersionValidityResponse
checkSchemaVersionValidityResponse pResponseStatus_ =
  CheckSchemaVersionValidityResponse'
    { _csvvrsError = Nothing,
      _csvvrsValid = Nothing,
      _csvvrsResponseStatus = pResponseStatus_
    }

-- | A validation failure error message.
csvvrsError :: Lens' CheckSchemaVersionValidityResponse (Maybe Text)
csvvrsError = lens _csvvrsError (\s a -> s {_csvvrsError = a})

-- | Return true, if the schema is valid and false otherwise.
csvvrsValid :: Lens' CheckSchemaVersionValidityResponse (Maybe Bool)
csvvrsValid = lens _csvvrsValid (\s a -> s {_csvvrsValid = a})

-- | -- | The response status code.
csvvrsResponseStatus :: Lens' CheckSchemaVersionValidityResponse Int
csvvrsResponseStatus = lens _csvvrsResponseStatus (\s a -> s {_csvvrsResponseStatus = a})

instance NFData CheckSchemaVersionValidityResponse
