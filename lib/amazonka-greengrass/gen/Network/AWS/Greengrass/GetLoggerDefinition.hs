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
-- Module      : Network.AWS.Greengrass.GetLoggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition.
module Network.AWS.Greengrass.GetLoggerDefinition
  ( -- * Creating a Request
    getLoggerDefinition,
    GetLoggerDefinition,

    -- * Request Lenses
    gldLoggerDefinitionId,

    -- * Destructuring the Response
    getLoggerDefinitionResponse,
    GetLoggerDefinitionResponse,

    -- * Response Lenses
    gldrsLatestVersionARN,
    gldrsARN,
    gldrsName,
    gldrsCreationTimestamp,
    gldrsId,
    gldrsLatestVersion,
    gldrsLastUpdatedTimestamp,
    gldrsTags,
    gldrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLoggerDefinition' smart constructor.
newtype GetLoggerDefinition = GetLoggerDefinition'
  { _gldLoggerDefinitionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetLoggerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gldLoggerDefinitionId' - The ID of the logger definition.
getLoggerDefinition ::
  -- | 'gldLoggerDefinitionId'
  Text ->
  GetLoggerDefinition
getLoggerDefinition pLoggerDefinitionId_ =
  GetLoggerDefinition'
    { _gldLoggerDefinitionId =
        pLoggerDefinitionId_
    }

-- | The ID of the logger definition.
gldLoggerDefinitionId :: Lens' GetLoggerDefinition Text
gldLoggerDefinitionId = lens _gldLoggerDefinitionId (\s a -> s {_gldLoggerDefinitionId = a})

instance AWSRequest GetLoggerDefinition where
  type Rs GetLoggerDefinition = GetLoggerDefinitionResponse
  request = get greengrass
  response =
    receiveJSON
      ( \s h x ->
          GetLoggerDefinitionResponse'
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

instance Hashable GetLoggerDefinition

instance NFData GetLoggerDefinition

instance ToHeaders GetLoggerDefinition where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetLoggerDefinition where
  toPath GetLoggerDefinition' {..} =
    mconcat
      ["/greengrass/definition/loggers/", toBS _gldLoggerDefinitionId]

instance ToQuery GetLoggerDefinition where
  toQuery = const mempty

-- | /See:/ 'getLoggerDefinitionResponse' smart constructor.
data GetLoggerDefinitionResponse = GetLoggerDefinitionResponse'
  { _gldrsLatestVersionARN ::
      !(Maybe Text),
    _gldrsARN :: !(Maybe Text),
    _gldrsName :: !(Maybe Text),
    _gldrsCreationTimestamp ::
      !(Maybe Text),
    _gldrsId :: !(Maybe Text),
    _gldrsLatestVersion ::
      !(Maybe Text),
    _gldrsLastUpdatedTimestamp ::
      !(Maybe Text),
    _gldrsTags ::
      !(Maybe (Map Text (Text))),
    _gldrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetLoggerDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gldrsLatestVersionARN' - The ARN of the latest version associated with the definition.
--
-- * 'gldrsARN' - The ARN of the definition.
--
-- * 'gldrsName' - The name of the definition.
--
-- * 'gldrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'gldrsId' - The ID of the definition.
--
-- * 'gldrsLatestVersion' - The ID of the latest version associated with the definition.
--
-- * 'gldrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'gldrsTags' - Tag(s) attached to the resource arn.
--
-- * 'gldrsResponseStatus' - -- | The response status code.
getLoggerDefinitionResponse ::
  -- | 'gldrsResponseStatus'
  Int ->
  GetLoggerDefinitionResponse
getLoggerDefinitionResponse pResponseStatus_ =
  GetLoggerDefinitionResponse'
    { _gldrsLatestVersionARN = Nothing,
      _gldrsARN = Nothing,
      _gldrsName = Nothing,
      _gldrsCreationTimestamp = Nothing,
      _gldrsId = Nothing,
      _gldrsLatestVersion = Nothing,
      _gldrsLastUpdatedTimestamp = Nothing,
      _gldrsTags = Nothing,
      _gldrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the latest version associated with the definition.
gldrsLatestVersionARN :: Lens' GetLoggerDefinitionResponse (Maybe Text)
gldrsLatestVersionARN = lens _gldrsLatestVersionARN (\s a -> s {_gldrsLatestVersionARN = a})

-- | The ARN of the definition.
gldrsARN :: Lens' GetLoggerDefinitionResponse (Maybe Text)
gldrsARN = lens _gldrsARN (\s a -> s {_gldrsARN = a})

-- | The name of the definition.
gldrsName :: Lens' GetLoggerDefinitionResponse (Maybe Text)
gldrsName = lens _gldrsName (\s a -> s {_gldrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
gldrsCreationTimestamp :: Lens' GetLoggerDefinitionResponse (Maybe Text)
gldrsCreationTimestamp = lens _gldrsCreationTimestamp (\s a -> s {_gldrsCreationTimestamp = a})

-- | The ID of the definition.
gldrsId :: Lens' GetLoggerDefinitionResponse (Maybe Text)
gldrsId = lens _gldrsId (\s a -> s {_gldrsId = a})

-- | The ID of the latest version associated with the definition.
gldrsLatestVersion :: Lens' GetLoggerDefinitionResponse (Maybe Text)
gldrsLatestVersion = lens _gldrsLatestVersion (\s a -> s {_gldrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
gldrsLastUpdatedTimestamp :: Lens' GetLoggerDefinitionResponse (Maybe Text)
gldrsLastUpdatedTimestamp = lens _gldrsLastUpdatedTimestamp (\s a -> s {_gldrsLastUpdatedTimestamp = a})

-- | Tag(s) attached to the resource arn.
gldrsTags :: Lens' GetLoggerDefinitionResponse (HashMap Text (Text))
gldrsTags = lens _gldrsTags (\s a -> s {_gldrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gldrsResponseStatus :: Lens' GetLoggerDefinitionResponse Int
gldrsResponseStatus = lens _gldrsResponseStatus (\s a -> s {_gldrsResponseStatus = a})

instance NFData GetLoggerDefinitionResponse
