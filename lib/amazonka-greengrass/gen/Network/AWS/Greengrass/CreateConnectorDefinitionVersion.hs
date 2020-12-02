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
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a connector definition which has already been defined.
module Network.AWS.Greengrass.CreateConnectorDefinitionVersion
  ( -- * Creating a Request
    createConnectorDefinitionVersion,
    CreateConnectorDefinitionVersion,

    -- * Request Lenses
    ccdvAmznClientToken,
    ccdvConnectors,
    ccdvConnectorDefinitionId,

    -- * Destructuring the Response
    createConnectorDefinitionVersionResponse,
    CreateConnectorDefinitionVersionResponse,

    -- * Response Lenses
    ccdvrsARN,
    ccdvrsCreationTimestamp,
    ccdvrsVersion,
    ccdvrsId,
    ccdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createConnectorDefinitionVersion' smart constructor.
data CreateConnectorDefinitionVersion = CreateConnectorDefinitionVersion'
  { _ccdvAmznClientToken ::
      !(Maybe Text),
    _ccdvConnectors ::
      !(Maybe [Connector]),
    _ccdvConnectorDefinitionId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateConnectorDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccdvAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'ccdvConnectors' - A list of references to connectors in this version, with their corresponding configuration settings.
--
-- * 'ccdvConnectorDefinitionId' - The ID of the connector definition.
createConnectorDefinitionVersion ::
  -- | 'ccdvConnectorDefinitionId'
  Text ->
  CreateConnectorDefinitionVersion
createConnectorDefinitionVersion pConnectorDefinitionId_ =
  CreateConnectorDefinitionVersion'
    { _ccdvAmznClientToken = Nothing,
      _ccdvConnectors = Nothing,
      _ccdvConnectorDefinitionId = pConnectorDefinitionId_
    }

-- | A client token used to correlate requests and responses.
ccdvAmznClientToken :: Lens' CreateConnectorDefinitionVersion (Maybe Text)
ccdvAmznClientToken = lens _ccdvAmznClientToken (\s a -> s {_ccdvAmznClientToken = a})

-- | A list of references to connectors in this version, with their corresponding configuration settings.
ccdvConnectors :: Lens' CreateConnectorDefinitionVersion [Connector]
ccdvConnectors = lens _ccdvConnectors (\s a -> s {_ccdvConnectors = a}) . _Default . _Coerce

-- | The ID of the connector definition.
ccdvConnectorDefinitionId :: Lens' CreateConnectorDefinitionVersion Text
ccdvConnectorDefinitionId = lens _ccdvConnectorDefinitionId (\s a -> s {_ccdvConnectorDefinitionId = a})

instance AWSRequest CreateConnectorDefinitionVersion where
  type
    Rs CreateConnectorDefinitionVersion =
      CreateConnectorDefinitionVersionResponse
  request = postJSON greengrass
  response =
    receiveJSON
      ( \s h x ->
          CreateConnectorDefinitionVersionResponse'
            <$> (x .?> "Arn")
            <*> (x .?> "CreationTimestamp")
            <*> (x .?> "Version")
            <*> (x .?> "Id")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateConnectorDefinitionVersion

instance NFData CreateConnectorDefinitionVersion

instance ToHeaders CreateConnectorDefinitionVersion where
  toHeaders CreateConnectorDefinitionVersion' {..} =
    mconcat
      [ "X-Amzn-Client-Token" =# _ccdvAmznClientToken,
        "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
      ]

instance ToJSON CreateConnectorDefinitionVersion where
  toJSON CreateConnectorDefinitionVersion' {..} =
    object (catMaybes [("Connectors" .=) <$> _ccdvConnectors])

instance ToPath CreateConnectorDefinitionVersion where
  toPath CreateConnectorDefinitionVersion' {..} =
    mconcat
      [ "/greengrass/definition/connectors/",
        toBS _ccdvConnectorDefinitionId,
        "/versions"
      ]

instance ToQuery CreateConnectorDefinitionVersion where
  toQuery = const mempty

-- | /See:/ 'createConnectorDefinitionVersionResponse' smart constructor.
data CreateConnectorDefinitionVersionResponse = CreateConnectorDefinitionVersionResponse'
  { _ccdvrsARN ::
      !( Maybe
           Text
       ),
    _ccdvrsCreationTimestamp ::
      !( Maybe
           Text
       ),
    _ccdvrsVersion ::
      !( Maybe
           Text
       ),
    _ccdvrsId ::
      !( Maybe
           Text
       ),
    _ccdvrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateConnectorDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccdvrsARN' - The ARN of the version.
--
-- * 'ccdvrsCreationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- * 'ccdvrsVersion' - The ID of the version.
--
-- * 'ccdvrsId' - The ID of the parent definition that the version is associated with.
--
-- * 'ccdvrsResponseStatus' - -- | The response status code.
createConnectorDefinitionVersionResponse ::
  -- | 'ccdvrsResponseStatus'
  Int ->
  CreateConnectorDefinitionVersionResponse
createConnectorDefinitionVersionResponse pResponseStatus_ =
  CreateConnectorDefinitionVersionResponse'
    { _ccdvrsARN = Nothing,
      _ccdvrsCreationTimestamp = Nothing,
      _ccdvrsVersion = Nothing,
      _ccdvrsId = Nothing,
      _ccdvrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the version.
ccdvrsARN :: Lens' CreateConnectorDefinitionVersionResponse (Maybe Text)
ccdvrsARN = lens _ccdvrsARN (\s a -> s {_ccdvrsARN = a})

-- | The time, in milliseconds since the epoch, when the version was created.
ccdvrsCreationTimestamp :: Lens' CreateConnectorDefinitionVersionResponse (Maybe Text)
ccdvrsCreationTimestamp = lens _ccdvrsCreationTimestamp (\s a -> s {_ccdvrsCreationTimestamp = a})

-- | The ID of the version.
ccdvrsVersion :: Lens' CreateConnectorDefinitionVersionResponse (Maybe Text)
ccdvrsVersion = lens _ccdvrsVersion (\s a -> s {_ccdvrsVersion = a})

-- | The ID of the parent definition that the version is associated with.
ccdvrsId :: Lens' CreateConnectorDefinitionVersionResponse (Maybe Text)
ccdvrsId = lens _ccdvrsId (\s a -> s {_ccdvrsId = a})

-- | -- | The response status code.
ccdvrsResponseStatus :: Lens' CreateConnectorDefinitionVersionResponse Int
ccdvrsResponseStatus = lens _ccdvrsResponseStatus (\s a -> s {_ccdvrsResponseStatus = a})

instance NFData CreateConnectorDefinitionVersionResponse
