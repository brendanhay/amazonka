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
-- Module      : Network.AWS.Connect.CreateInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates an Amazon Connect instance with all the supported channels enabled. It does not attach any storage (such as Amazon S3, or Kinesis) or allow for any configurations on features such as Contact Lens for Amazon Connect.
module Network.AWS.Connect.CreateInstance
  ( -- * Creating a Request
    createInstance,
    CreateInstance,

    -- * Request Lenses
    ciDirectoryId,
    ciClientToken,
    ciInstanceAlias,
    ciIdentityManagementType,
    ciInboundCallsEnabled,
    ciOutboundCallsEnabled,

    -- * Destructuring the Response
    createInstanceResponse,
    CreateInstanceResponse,

    -- * Response Lenses
    cirsARN,
    cirsId,
    cirsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createInstance' smart constructor.
data CreateInstance = CreateInstance'
  { _ciDirectoryId ::
      !(Maybe Text),
    _ciClientToken :: !(Maybe Text),
    _ciInstanceAlias :: !(Maybe (Sensitive Text)),
    _ciIdentityManagementType :: !DirectoryType,
    _ciInboundCallsEnabled :: !Bool,
    _ciOutboundCallsEnabled :: !Bool
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciDirectoryId' - The identifier for the directory.
--
-- * 'ciClientToken' - The idempotency token.
--
-- * 'ciInstanceAlias' - The name for your instance.
--
-- * 'ciIdentityManagementType' - The type of identity management for your Amazon Connect users.
--
-- * 'ciInboundCallsEnabled' - Whether your contact center handles incoming contacts.
--
-- * 'ciOutboundCallsEnabled' - Whether your contact center allows outbound calls.
createInstance ::
  -- | 'ciIdentityManagementType'
  DirectoryType ->
  -- | 'ciInboundCallsEnabled'
  Bool ->
  -- | 'ciOutboundCallsEnabled'
  Bool ->
  CreateInstance
createInstance
  pIdentityManagementType_
  pInboundCallsEnabled_
  pOutboundCallsEnabled_ =
    CreateInstance'
      { _ciDirectoryId = Nothing,
        _ciClientToken = Nothing,
        _ciInstanceAlias = Nothing,
        _ciIdentityManagementType = pIdentityManagementType_,
        _ciInboundCallsEnabled = pInboundCallsEnabled_,
        _ciOutboundCallsEnabled = pOutboundCallsEnabled_
      }

-- | The identifier for the directory.
ciDirectoryId :: Lens' CreateInstance (Maybe Text)
ciDirectoryId = lens _ciDirectoryId (\s a -> s {_ciDirectoryId = a})

-- | The idempotency token.
ciClientToken :: Lens' CreateInstance (Maybe Text)
ciClientToken = lens _ciClientToken (\s a -> s {_ciClientToken = a})

-- | The name for your instance.
ciInstanceAlias :: Lens' CreateInstance (Maybe Text)
ciInstanceAlias = lens _ciInstanceAlias (\s a -> s {_ciInstanceAlias = a}) . mapping _Sensitive

-- | The type of identity management for your Amazon Connect users.
ciIdentityManagementType :: Lens' CreateInstance DirectoryType
ciIdentityManagementType = lens _ciIdentityManagementType (\s a -> s {_ciIdentityManagementType = a})

-- | Whether your contact center handles incoming contacts.
ciInboundCallsEnabled :: Lens' CreateInstance Bool
ciInboundCallsEnabled = lens _ciInboundCallsEnabled (\s a -> s {_ciInboundCallsEnabled = a})

-- | Whether your contact center allows outbound calls.
ciOutboundCallsEnabled :: Lens' CreateInstance Bool
ciOutboundCallsEnabled = lens _ciOutboundCallsEnabled (\s a -> s {_ciOutboundCallsEnabled = a})

instance AWSRequest CreateInstance where
  type Rs CreateInstance = CreateInstanceResponse
  request = putJSON connect
  response =
    receiveJSON
      ( \s h x ->
          CreateInstanceResponse'
            <$> (x .?> "Arn") <*> (x .?> "Id") <*> (pure (fromEnum s))
      )

instance Hashable CreateInstance

instance NFData CreateInstance

instance ToHeaders CreateInstance where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateInstance where
  toJSON CreateInstance' {..} =
    object
      ( catMaybes
          [ ("DirectoryId" .=) <$> _ciDirectoryId,
            ("ClientToken" .=) <$> _ciClientToken,
            ("InstanceAlias" .=) <$> _ciInstanceAlias,
            Just ("IdentityManagementType" .= _ciIdentityManagementType),
            Just ("InboundCallsEnabled" .= _ciInboundCallsEnabled),
            Just ("OutboundCallsEnabled" .= _ciOutboundCallsEnabled)
          ]
      )

instance ToPath CreateInstance where
  toPath = const "/instance"

instance ToQuery CreateInstance where
  toQuery = const mempty

-- | /See:/ 'createInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { _cirsARN ::
      !(Maybe Text),
    _cirsId :: !(Maybe Text),
    _cirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirsARN' - The Amazon Resource Name (ARN) of the instance.
--
-- * 'cirsId' - The identifier for the instance.
--
-- * 'cirsResponseStatus' - -- | The response status code.
createInstanceResponse ::
  -- | 'cirsResponseStatus'
  Int ->
  CreateInstanceResponse
createInstanceResponse pResponseStatus_ =
  CreateInstanceResponse'
    { _cirsARN = Nothing,
      _cirsId = Nothing,
      _cirsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the instance.
cirsARN :: Lens' CreateInstanceResponse (Maybe Text)
cirsARN = lens _cirsARN (\s a -> s {_cirsARN = a})

-- | The identifier for the instance.
cirsId :: Lens' CreateInstanceResponse (Maybe Text)
cirsId = lens _cirsId (\s a -> s {_cirsId = a})

-- | -- | The response status code.
cirsResponseStatus :: Lens' CreateInstanceResponse Int
cirsResponseStatus = lens _cirsResponseStatus (\s a -> s {_cirsResponseStatus = a})

instance NFData CreateInstanceResponse
