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
-- Module      : Network.AWS.Greengrass.GetCoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinitionVersion
  ( -- * Creating a Request
    getCoreDefinitionVersion,
    GetCoreDefinitionVersion,

    -- * Request Lenses
    gcdvCoreDefinitionId,
    gcdvCoreDefinitionVersionId,

    -- * Destructuring the Response
    getCoreDefinitionVersionResponse,
    GetCoreDefinitionVersionResponse,

    -- * Response Lenses
    getrsDefinition,
    getrsARN,
    getrsNextToken,
    getrsCreationTimestamp,
    getrsVersion,
    getrsId,
    getrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCoreDefinitionVersion' smart constructor.
data GetCoreDefinitionVersion = GetCoreDefinitionVersion'
  { _gcdvCoreDefinitionId ::
      !Text,
    _gcdvCoreDefinitionVersionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCoreDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdvCoreDefinitionId' - The ID of the core definition.
--
-- * 'gcdvCoreDefinitionVersionId' - The ID of the core definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListCoreDefinitionVersions'' requests. If the version is the last one that was associated with a core definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
getCoreDefinitionVersion ::
  -- | 'gcdvCoreDefinitionId'
  Text ->
  -- | 'gcdvCoreDefinitionVersionId'
  Text ->
  GetCoreDefinitionVersion
getCoreDefinitionVersion
  pCoreDefinitionId_
  pCoreDefinitionVersionId_ =
    GetCoreDefinitionVersion'
      { _gcdvCoreDefinitionId =
          pCoreDefinitionId_,
        _gcdvCoreDefinitionVersionId = pCoreDefinitionVersionId_
      }

-- | The ID of the core definition.
gcdvCoreDefinitionId :: Lens' GetCoreDefinitionVersion Text
gcdvCoreDefinitionId = lens _gcdvCoreDefinitionId (\s a -> s {_gcdvCoreDefinitionId = a})

-- | The ID of the core definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListCoreDefinitionVersions'' requests. If the version is the last one that was associated with a core definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
gcdvCoreDefinitionVersionId :: Lens' GetCoreDefinitionVersion Text
gcdvCoreDefinitionVersionId = lens _gcdvCoreDefinitionVersionId (\s a -> s {_gcdvCoreDefinitionVersionId = a})

instance AWSRequest GetCoreDefinitionVersion where
  type Rs GetCoreDefinitionVersion = GetCoreDefinitionVersionResponse
  request = get greengrass
  response =
    receiveJSON
      ( \s h x ->
          GetCoreDefinitionVersionResponse'
            <$> (x .?> "Definition")
            <*> (x .?> "Arn")
            <*> (x .?> "NextToken")
            <*> (x .?> "CreationTimestamp")
            <*> (x .?> "Version")
            <*> (x .?> "Id")
            <*> (pure (fromEnum s))
      )

instance Hashable GetCoreDefinitionVersion

instance NFData GetCoreDefinitionVersion

instance ToHeaders GetCoreDefinitionVersion where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetCoreDefinitionVersion where
  toPath GetCoreDefinitionVersion' {..} =
    mconcat
      [ "/greengrass/definition/cores/",
        toBS _gcdvCoreDefinitionId,
        "/versions/",
        toBS _gcdvCoreDefinitionVersionId
      ]

instance ToQuery GetCoreDefinitionVersion where
  toQuery = const mempty

-- | /See:/ 'getCoreDefinitionVersionResponse' smart constructor.
data GetCoreDefinitionVersionResponse = GetCoreDefinitionVersionResponse'
  { _getrsDefinition ::
      !( Maybe
           CoreDefinitionVersion
       ),
    _getrsARN ::
      !(Maybe Text),
    _getrsNextToken ::
      !(Maybe Text),
    _getrsCreationTimestamp ::
      !(Maybe Text),
    _getrsVersion ::
      !(Maybe Text),
    _getrsId :: !(Maybe Text),
    _getrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCoreDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsDefinition' - Information about the core definition version.
--
-- * 'getrsARN' - The ARN of the core definition version.
--
-- * 'getrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'getrsCreationTimestamp' - The time, in milliseconds since the epoch, when the core definition version was created.
--
-- * 'getrsVersion' - The version of the core definition version.
--
-- * 'getrsId' - The ID of the core definition version.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getCoreDefinitionVersionResponse ::
  -- | 'getrsResponseStatus'
  Int ->
  GetCoreDefinitionVersionResponse
getCoreDefinitionVersionResponse pResponseStatus_ =
  GetCoreDefinitionVersionResponse'
    { _getrsDefinition = Nothing,
      _getrsARN = Nothing,
      _getrsNextToken = Nothing,
      _getrsCreationTimestamp = Nothing,
      _getrsVersion = Nothing,
      _getrsId = Nothing,
      _getrsResponseStatus = pResponseStatus_
    }

-- | Information about the core definition version.
getrsDefinition :: Lens' GetCoreDefinitionVersionResponse (Maybe CoreDefinitionVersion)
getrsDefinition = lens _getrsDefinition (\s a -> s {_getrsDefinition = a})

-- | The ARN of the core definition version.
getrsARN :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
getrsARN = lens _getrsARN (\s a -> s {_getrsARN = a})

-- | The token for the next set of results, or ''null'' if there are no additional results.
getrsNextToken :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
getrsNextToken = lens _getrsNextToken (\s a -> s {_getrsNextToken = a})

-- | The time, in milliseconds since the epoch, when the core definition version was created.
getrsCreationTimestamp :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
getrsCreationTimestamp = lens _getrsCreationTimestamp (\s a -> s {_getrsCreationTimestamp = a})

-- | The version of the core definition version.
getrsVersion :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
getrsVersion = lens _getrsVersion (\s a -> s {_getrsVersion = a})

-- | The ID of the core definition version.
getrsId :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
getrsId = lens _getrsId (\s a -> s {_getrsId = a})

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetCoreDefinitionVersionResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\s a -> s {_getrsResponseStatus = a})

instance NFData GetCoreDefinitionVersionResponse
