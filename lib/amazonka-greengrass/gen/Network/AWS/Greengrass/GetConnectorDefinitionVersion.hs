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
-- Module      : Network.AWS.Greengrass.GetConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition version, including the connectors that the version contains. Connectors are prebuilt modules that interact with local infrastructure, device protocols, AWS, and other cloud services.
module Network.AWS.Greengrass.GetConnectorDefinitionVersion
  ( -- * Creating a Request
    getConnectorDefinitionVersion,
    GetConnectorDefinitionVersion,

    -- * Request Lenses
    gcdvNextToken,
    gcdvConnectorDefinitionId,
    gcdvConnectorDefinitionVersionId,

    -- * Destructuring the Response
    getConnectorDefinitionVersionResponse,
    GetConnectorDefinitionVersionResponse,

    -- * Response Lenses
    gcdvrsDefinition,
    gcdvrsARN,
    gcdvrsNextToken,
    gcdvrsCreationTimestamp,
    gcdvrsVersion,
    gcdvrsId,
    gcdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConnectorDefinitionVersion' smart constructor.
data GetConnectorDefinitionVersion = GetConnectorDefinitionVersion'
  { _gcdvNextToken ::
      !(Maybe Text),
    _gcdvConnectorDefinitionId ::
      !Text,
    _gcdvConnectorDefinitionVersionId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectorDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdvNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'gcdvConnectorDefinitionId' - The ID of the connector definition.
--
-- * 'gcdvConnectorDefinitionVersionId' - The ID of the connector definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListConnectorDefinitionVersions'' requests. If the version is the last one that was associated with a connector definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
getConnectorDefinitionVersion ::
  -- | 'gcdvConnectorDefinitionId'
  Text ->
  -- | 'gcdvConnectorDefinitionVersionId'
  Text ->
  GetConnectorDefinitionVersion
getConnectorDefinitionVersion
  pConnectorDefinitionId_
  pConnectorDefinitionVersionId_ =
    GetConnectorDefinitionVersion'
      { _gcdvNextToken = Nothing,
        _gcdvConnectorDefinitionId = pConnectorDefinitionId_,
        _gcdvConnectorDefinitionVersionId =
          pConnectorDefinitionVersionId_
      }

-- | The token for the next set of results, or ''null'' if there are no additional results.
gcdvNextToken :: Lens' GetConnectorDefinitionVersion (Maybe Text)
gcdvNextToken = lens _gcdvNextToken (\s a -> s {_gcdvNextToken = a})

-- | The ID of the connector definition.
gcdvConnectorDefinitionId :: Lens' GetConnectorDefinitionVersion Text
gcdvConnectorDefinitionId = lens _gcdvConnectorDefinitionId (\s a -> s {_gcdvConnectorDefinitionId = a})

-- | The ID of the connector definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListConnectorDefinitionVersions'' requests. If the version is the last one that was associated with a connector definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
gcdvConnectorDefinitionVersionId :: Lens' GetConnectorDefinitionVersion Text
gcdvConnectorDefinitionVersionId = lens _gcdvConnectorDefinitionVersionId (\s a -> s {_gcdvConnectorDefinitionVersionId = a})

instance AWSRequest GetConnectorDefinitionVersion where
  type
    Rs GetConnectorDefinitionVersion =
      GetConnectorDefinitionVersionResponse
  request = get greengrass
  response =
    receiveJSON
      ( \s h x ->
          GetConnectorDefinitionVersionResponse'
            <$> (x .?> "Definition")
            <*> (x .?> "Arn")
            <*> (x .?> "NextToken")
            <*> (x .?> "CreationTimestamp")
            <*> (x .?> "Version")
            <*> (x .?> "Id")
            <*> (pure (fromEnum s))
      )

instance Hashable GetConnectorDefinitionVersion

instance NFData GetConnectorDefinitionVersion

instance ToHeaders GetConnectorDefinitionVersion where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetConnectorDefinitionVersion where
  toPath GetConnectorDefinitionVersion' {..} =
    mconcat
      [ "/greengrass/definition/connectors/",
        toBS _gcdvConnectorDefinitionId,
        "/versions/",
        toBS _gcdvConnectorDefinitionVersionId
      ]

instance ToQuery GetConnectorDefinitionVersion where
  toQuery GetConnectorDefinitionVersion' {..} =
    mconcat ["NextToken" =: _gcdvNextToken]

-- | /See:/ 'getConnectorDefinitionVersionResponse' smart constructor.
data GetConnectorDefinitionVersionResponse = GetConnectorDefinitionVersionResponse'
  { _gcdvrsDefinition ::
      !( Maybe
           ConnectorDefinitionVersion
       ),
    _gcdvrsARN ::
      !(Maybe Text),
    _gcdvrsNextToken ::
      !(Maybe Text),
    _gcdvrsCreationTimestamp ::
      !(Maybe Text),
    _gcdvrsVersion ::
      !(Maybe Text),
    _gcdvrsId ::
      !(Maybe Text),
    _gcdvrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectorDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdvrsDefinition' - Information about the connector definition version.
--
-- * 'gcdvrsARN' - The ARN of the connector definition version.
--
-- * 'gcdvrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'gcdvrsCreationTimestamp' - The time, in milliseconds since the epoch, when the connector definition version was created.
--
-- * 'gcdvrsVersion' - The version of the connector definition version.
--
-- * 'gcdvrsId' - The ID of the connector definition version.
--
-- * 'gcdvrsResponseStatus' - -- | The response status code.
getConnectorDefinitionVersionResponse ::
  -- | 'gcdvrsResponseStatus'
  Int ->
  GetConnectorDefinitionVersionResponse
getConnectorDefinitionVersionResponse pResponseStatus_ =
  GetConnectorDefinitionVersionResponse'
    { _gcdvrsDefinition =
        Nothing,
      _gcdvrsARN = Nothing,
      _gcdvrsNextToken = Nothing,
      _gcdvrsCreationTimestamp = Nothing,
      _gcdvrsVersion = Nothing,
      _gcdvrsId = Nothing,
      _gcdvrsResponseStatus = pResponseStatus_
    }

-- | Information about the connector definition version.
gcdvrsDefinition :: Lens' GetConnectorDefinitionVersionResponse (Maybe ConnectorDefinitionVersion)
gcdvrsDefinition = lens _gcdvrsDefinition (\s a -> s {_gcdvrsDefinition = a})

-- | The ARN of the connector definition version.
gcdvrsARN :: Lens' GetConnectorDefinitionVersionResponse (Maybe Text)
gcdvrsARN = lens _gcdvrsARN (\s a -> s {_gcdvrsARN = a})

-- | The token for the next set of results, or ''null'' if there are no additional results.
gcdvrsNextToken :: Lens' GetConnectorDefinitionVersionResponse (Maybe Text)
gcdvrsNextToken = lens _gcdvrsNextToken (\s a -> s {_gcdvrsNextToken = a})

-- | The time, in milliseconds since the epoch, when the connector definition version was created.
gcdvrsCreationTimestamp :: Lens' GetConnectorDefinitionVersionResponse (Maybe Text)
gcdvrsCreationTimestamp = lens _gcdvrsCreationTimestamp (\s a -> s {_gcdvrsCreationTimestamp = a})

-- | The version of the connector definition version.
gcdvrsVersion :: Lens' GetConnectorDefinitionVersionResponse (Maybe Text)
gcdvrsVersion = lens _gcdvrsVersion (\s a -> s {_gcdvrsVersion = a})

-- | The ID of the connector definition version.
gcdvrsId :: Lens' GetConnectorDefinitionVersionResponse (Maybe Text)
gcdvrsId = lens _gcdvrsId (\s a -> s {_gcdvrsId = a})

-- | -- | The response status code.
gcdvrsResponseStatus :: Lens' GetConnectorDefinitionVersionResponse Int
gcdvrsResponseStatus = lens _gcdvrsResponseStatus (\s a -> s {_gcdvrsResponseStatus = a})

instance NFData GetConnectorDefinitionVersionResponse
