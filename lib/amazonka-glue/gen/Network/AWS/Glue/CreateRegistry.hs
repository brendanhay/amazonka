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
-- Module      : Network.AWS.Glue.CreateRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new registry which may be used to hold a collection of schemas.
module Network.AWS.Glue.CreateRegistry
  ( -- * Creating a Request
    createRegistry,
    CreateRegistry,

    -- * Request Lenses
    crDescription,
    crTags,
    crRegistryName,

    -- * Destructuring the Response
    createRegistryResponse,
    CreateRegistryResponse,

    -- * Response Lenses
    crrsRegistryName,
    crrsRegistryARN,
    crrsDescription,
    crrsTags,
    crrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRegistry' smart constructor.
data CreateRegistry = CreateRegistry'
  { _crDescription ::
      !(Maybe Text),
    _crTags :: !(Maybe (Map Text (Text))),
    _crRegistryName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRegistry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crDescription' - A description of the registry. If description is not provided, there will not be any default value for this.
--
-- * 'crTags' - AWS tags that contain a key value pair and may be searched by console, command line, or API.
--
-- * 'crRegistryName' - Name of the registry to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
createRegistry ::
  -- | 'crRegistryName'
  Text ->
  CreateRegistry
createRegistry pRegistryName_ =
  CreateRegistry'
    { _crDescription = Nothing,
      _crTags = Nothing,
      _crRegistryName = pRegistryName_
    }

-- | A description of the registry. If description is not provided, there will not be any default value for this.
crDescription :: Lens' CreateRegistry (Maybe Text)
crDescription = lens _crDescription (\s a -> s {_crDescription = a})

-- | AWS tags that contain a key value pair and may be searched by console, command line, or API.
crTags :: Lens' CreateRegistry (HashMap Text (Text))
crTags = lens _crTags (\s a -> s {_crTags = a}) . _Default . _Map

-- | Name of the registry to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
crRegistryName :: Lens' CreateRegistry Text
crRegistryName = lens _crRegistryName (\s a -> s {_crRegistryName = a})

instance AWSRequest CreateRegistry where
  type Rs CreateRegistry = CreateRegistryResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          CreateRegistryResponse'
            <$> (x .?> "RegistryName")
            <*> (x .?> "RegistryArn")
            <*> (x .?> "Description")
            <*> (x .?> "Tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable CreateRegistry

instance NFData CreateRegistry

instance ToHeaders CreateRegistry where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.CreateRegistry" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateRegistry where
  toJSON CreateRegistry' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _crDescription,
            ("Tags" .=) <$> _crTags,
            Just ("RegistryName" .= _crRegistryName)
          ]
      )

instance ToPath CreateRegistry where
  toPath = const "/"

instance ToQuery CreateRegistry where
  toQuery = const mempty

-- | /See:/ 'createRegistryResponse' smart constructor.
data CreateRegistryResponse = CreateRegistryResponse'
  { _crrsRegistryName ::
      !(Maybe Text),
    _crrsRegistryARN :: !(Maybe Text),
    _crrsDescription :: !(Maybe Text),
    _crrsTags :: !(Maybe (Map Text (Text))),
    _crrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRegistryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRegistryName' - The name of the registry.
--
-- * 'crrsRegistryARN' - The Amazon Resource Name (ARN) of the newly created registry.
--
-- * 'crrsDescription' - A description of the registry.
--
-- * 'crrsTags' - The tags for the registry.
--
-- * 'crrsResponseStatus' - -- | The response status code.
createRegistryResponse ::
  -- | 'crrsResponseStatus'
  Int ->
  CreateRegistryResponse
createRegistryResponse pResponseStatus_ =
  CreateRegistryResponse'
    { _crrsRegistryName = Nothing,
      _crrsRegistryARN = Nothing,
      _crrsDescription = Nothing,
      _crrsTags = Nothing,
      _crrsResponseStatus = pResponseStatus_
    }

-- | The name of the registry.
crrsRegistryName :: Lens' CreateRegistryResponse (Maybe Text)
crrsRegistryName = lens _crrsRegistryName (\s a -> s {_crrsRegistryName = a})

-- | The Amazon Resource Name (ARN) of the newly created registry.
crrsRegistryARN :: Lens' CreateRegistryResponse (Maybe Text)
crrsRegistryARN = lens _crrsRegistryARN (\s a -> s {_crrsRegistryARN = a})

-- | A description of the registry.
crrsDescription :: Lens' CreateRegistryResponse (Maybe Text)
crrsDescription = lens _crrsDescription (\s a -> s {_crrsDescription = a})

-- | The tags for the registry.
crrsTags :: Lens' CreateRegistryResponse (HashMap Text (Text))
crrsTags = lens _crrsTags (\s a -> s {_crrsTags = a}) . _Default . _Map

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateRegistryResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\s a -> s {_crrsResponseStatus = a})

instance NFData CreateRegistryResponse
