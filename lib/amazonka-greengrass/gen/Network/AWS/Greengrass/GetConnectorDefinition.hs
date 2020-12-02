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
-- Module      : Network.AWS.Greengrass.GetConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition.
module Network.AWS.Greengrass.GetConnectorDefinition
  ( -- * Creating a Request
    getConnectorDefinition,
    GetConnectorDefinition,

    -- * Request Lenses
    gcdConnectorDefinitionId,

    -- * Destructuring the Response
    getConnectorDefinitionResponse,
    GetConnectorDefinitionResponse,

    -- * Response Lenses
    grsLatestVersionARN,
    grsARN,
    grsName,
    grsCreationTimestamp,
    grsId,
    grsLatestVersion,
    grsLastUpdatedTimestamp,
    grsTags,
    grsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConnectorDefinition' smart constructor.
newtype GetConnectorDefinition = GetConnectorDefinition'
  { _gcdConnectorDefinitionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectorDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdConnectorDefinitionId' - The ID of the connector definition.
getConnectorDefinition ::
  -- | 'gcdConnectorDefinitionId'
  Text ->
  GetConnectorDefinition
getConnectorDefinition pConnectorDefinitionId_ =
  GetConnectorDefinition'
    { _gcdConnectorDefinitionId =
        pConnectorDefinitionId_
    }

-- | The ID of the connector definition.
gcdConnectorDefinitionId :: Lens' GetConnectorDefinition Text
gcdConnectorDefinitionId = lens _gcdConnectorDefinitionId (\s a -> s {_gcdConnectorDefinitionId = a})

instance AWSRequest GetConnectorDefinition where
  type Rs GetConnectorDefinition = GetConnectorDefinitionResponse
  request = get greengrass
  response =
    receiveJSON
      ( \s h x ->
          GetConnectorDefinitionResponse'
            <$> (x .?> "LatestVersionArn")
            <*> (x .?> "Arn")
            <*> (x .?> "Name")
            <*> (x .?> "CreationTimestamp")
            <*> (x .?> "Id")
            <*> (x .?> "LatestVersion")
            <*> (x .?> "LastUpdatedTimestamp")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetConnectorDefinition

instance NFData GetConnectorDefinition

instance ToHeaders GetConnectorDefinition where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetConnectorDefinition where
  toPath GetConnectorDefinition' {..} =
    mconcat
      [ "/greengrass/definition/connectors/",
        toBS _gcdConnectorDefinitionId
      ]

instance ToQuery GetConnectorDefinition where
  toQuery = const mempty

-- | /See:/ 'getConnectorDefinitionResponse' smart constructor.
data GetConnectorDefinitionResponse = GetConnectorDefinitionResponse'
  { _grsLatestVersionARN ::
      !(Maybe Text),
    _grsARN :: !(Maybe Text),
    _grsName :: !(Maybe Text),
    _grsCreationTimestamp ::
      !(Maybe Text),
    _grsId :: !(Maybe Text),
    _grsLatestVersion ::
      !(Maybe Text),
    _grsLastUpdatedTimestamp ::
      !(Maybe Text),
    _grsTags ::
      !(Maybe (Map Text (Text))),
    _grsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsLatestVersionARN' - The ARN of the latest version associated with the definition.
--
-- * 'grsARN' - The ARN of the definition.
--
-- * 'grsName' - The name of the definition.
--
-- * 'grsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'grsId' - The ID of the definition.
--
-- * 'grsLatestVersion' - The ID of the latest version associated with the definition.
--
-- * 'grsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'grsTags' - Tag(s) attached to the resource arn.
--
-- * 'grsResponseStatus' - -- | The response status code.
getConnectorDefinitionResponse ::
  -- | 'grsResponseStatus'
  Int ->
  GetConnectorDefinitionResponse
getConnectorDefinitionResponse pResponseStatus_ =
  GetConnectorDefinitionResponse'
    { _grsLatestVersionARN = Nothing,
      _grsARN = Nothing,
      _grsName = Nothing,
      _grsCreationTimestamp = Nothing,
      _grsId = Nothing,
      _grsLatestVersion = Nothing,
      _grsLastUpdatedTimestamp = Nothing,
      _grsTags = Nothing,
      _grsResponseStatus = pResponseStatus_
    }

-- | The ARN of the latest version associated with the definition.
grsLatestVersionARN :: Lens' GetConnectorDefinitionResponse (Maybe Text)
grsLatestVersionARN = lens _grsLatestVersionARN (\s a -> s {_grsLatestVersionARN = a})

-- | The ARN of the definition.
grsARN :: Lens' GetConnectorDefinitionResponse (Maybe Text)
grsARN = lens _grsARN (\s a -> s {_grsARN = a})

-- | The name of the definition.
grsName :: Lens' GetConnectorDefinitionResponse (Maybe Text)
grsName = lens _grsName (\s a -> s {_grsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
grsCreationTimestamp :: Lens' GetConnectorDefinitionResponse (Maybe Text)
grsCreationTimestamp = lens _grsCreationTimestamp (\s a -> s {_grsCreationTimestamp = a})

-- | The ID of the definition.
grsId :: Lens' GetConnectorDefinitionResponse (Maybe Text)
grsId = lens _grsId (\s a -> s {_grsId = a})

-- | The ID of the latest version associated with the definition.
grsLatestVersion :: Lens' GetConnectorDefinitionResponse (Maybe Text)
grsLatestVersion = lens _grsLatestVersion (\s a -> s {_grsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
grsLastUpdatedTimestamp :: Lens' GetConnectorDefinitionResponse (Maybe Text)
grsLastUpdatedTimestamp = lens _grsLastUpdatedTimestamp (\s a -> s {_grsLastUpdatedTimestamp = a})

-- | Tag(s) attached to the resource arn.
grsTags :: Lens' GetConnectorDefinitionResponse (HashMap Text (Text))
grsTags = lens _grsTags (\s a -> s {_grsTags = a}) . _Default . _Map

-- | -- | The response status code.
grsResponseStatus :: Lens' GetConnectorDefinitionResponse Int
grsResponseStatus = lens _grsResponseStatus (\s a -> s {_grsResponseStatus = a})

instance NFData GetConnectorDefinitionResponse
