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
-- Module      : Network.AWS.Greengrass.CreateCoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a core definition that has already been defined. Greengrass groups must each contain exactly one Greengrass core.
module Network.AWS.Greengrass.CreateCoreDefinitionVersion
  ( -- * Creating a Request
    createCoreDefinitionVersion,
    CreateCoreDefinitionVersion,

    -- * Request Lenses
    creAmznClientToken,
    creCores,
    creCoreDefinitionId,

    -- * Destructuring the Response
    createCoreDefinitionVersionResponse,
    CreateCoreDefinitionVersionResponse,

    -- * Response Lenses
    crsARN,
    crsCreationTimestamp,
    crsVersion,
    crsId,
    crsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCoreDefinitionVersion' smart constructor.
data CreateCoreDefinitionVersion = CreateCoreDefinitionVersion'
  { _creAmznClientToken ::
      !(Maybe Text),
    _creCores :: !(Maybe [Core]),
    _creCoreDefinitionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCoreDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'creCores' - A list of cores in the core definition version.
--
-- * 'creCoreDefinitionId' - The ID of the core definition.
createCoreDefinitionVersion ::
  -- | 'creCoreDefinitionId'
  Text ->
  CreateCoreDefinitionVersion
createCoreDefinitionVersion pCoreDefinitionId_ =
  CreateCoreDefinitionVersion'
    { _creAmznClientToken = Nothing,
      _creCores = Nothing,
      _creCoreDefinitionId = pCoreDefinitionId_
    }

-- | A client token used to correlate requests and responses.
creAmznClientToken :: Lens' CreateCoreDefinitionVersion (Maybe Text)
creAmznClientToken = lens _creAmznClientToken (\s a -> s {_creAmznClientToken = a})

-- | A list of cores in the core definition version.
creCores :: Lens' CreateCoreDefinitionVersion [Core]
creCores = lens _creCores (\s a -> s {_creCores = a}) . _Default . _Coerce

-- | The ID of the core definition.
creCoreDefinitionId :: Lens' CreateCoreDefinitionVersion Text
creCoreDefinitionId = lens _creCoreDefinitionId (\s a -> s {_creCoreDefinitionId = a})

instance AWSRequest CreateCoreDefinitionVersion where
  type
    Rs CreateCoreDefinitionVersion =
      CreateCoreDefinitionVersionResponse
  request = postJSON greengrass
  response =
    receiveJSON
      ( \s h x ->
          CreateCoreDefinitionVersionResponse'
            <$> (x .?> "Arn")
            <*> (x .?> "CreationTimestamp")
            <*> (x .?> "Version")
            <*> (x .?> "Id")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateCoreDefinitionVersion

instance NFData CreateCoreDefinitionVersion

instance ToHeaders CreateCoreDefinitionVersion where
  toHeaders CreateCoreDefinitionVersion' {..} =
    mconcat
      [ "X-Amzn-Client-Token" =# _creAmznClientToken,
        "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
      ]

instance ToJSON CreateCoreDefinitionVersion where
  toJSON CreateCoreDefinitionVersion' {..} =
    object (catMaybes [("Cores" .=) <$> _creCores])

instance ToPath CreateCoreDefinitionVersion where
  toPath CreateCoreDefinitionVersion' {..} =
    mconcat
      [ "/greengrass/definition/cores/",
        toBS _creCoreDefinitionId,
        "/versions"
      ]

instance ToQuery CreateCoreDefinitionVersion where
  toQuery = const mempty

-- | /See:/ 'createCoreDefinitionVersionResponse' smart constructor.
data CreateCoreDefinitionVersionResponse = CreateCoreDefinitionVersionResponse'
  { _crsARN ::
      !(Maybe Text),
    _crsCreationTimestamp ::
      !(Maybe Text),
    _crsVersion ::
      !(Maybe Text),
    _crsId ::
      !(Maybe Text),
    _crsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCoreDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsARN' - The ARN of the version.
--
-- * 'crsCreationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- * 'crsVersion' - The ID of the version.
--
-- * 'crsId' - The ID of the parent definition that the version is associated with.
--
-- * 'crsResponseStatus' - -- | The response status code.
createCoreDefinitionVersionResponse ::
  -- | 'crsResponseStatus'
  Int ->
  CreateCoreDefinitionVersionResponse
createCoreDefinitionVersionResponse pResponseStatus_ =
  CreateCoreDefinitionVersionResponse'
    { _crsARN = Nothing,
      _crsCreationTimestamp = Nothing,
      _crsVersion = Nothing,
      _crsId = Nothing,
      _crsResponseStatus = pResponseStatus_
    }

-- | The ARN of the version.
crsARN :: Lens' CreateCoreDefinitionVersionResponse (Maybe Text)
crsARN = lens _crsARN (\s a -> s {_crsARN = a})

-- | The time, in milliseconds since the epoch, when the version was created.
crsCreationTimestamp :: Lens' CreateCoreDefinitionVersionResponse (Maybe Text)
crsCreationTimestamp = lens _crsCreationTimestamp (\s a -> s {_crsCreationTimestamp = a})

-- | The ID of the version.
crsVersion :: Lens' CreateCoreDefinitionVersionResponse (Maybe Text)
crsVersion = lens _crsVersion (\s a -> s {_crsVersion = a})

-- | The ID of the parent definition that the version is associated with.
crsId :: Lens' CreateCoreDefinitionVersionResponse (Maybe Text)
crsId = lens _crsId (\s a -> s {_crsId = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateCoreDefinitionVersionResponse Int
crsResponseStatus = lens _crsResponseStatus (\s a -> s {_crsResponseStatus = a})

instance NFData CreateCoreDefinitionVersionResponse
