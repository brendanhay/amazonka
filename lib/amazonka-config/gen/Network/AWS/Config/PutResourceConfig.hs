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
-- Module      : Network.AWS.Config.PutResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for the resource provided in the request. The configuration state of a resource is represented in AWS Config as Configuration Items. Once this API records the configuration item, you can retrieve the list of configuration items for the custom resource type using existing AWS Config APIs.
module Network.AWS.Config.PutResourceConfig
  ( -- * Creating a Request
    putResourceConfig,
    PutResourceConfig,

    -- * Request Lenses
    prcResourceName,
    prcTags,
    prcResourceType,
    prcSchemaVersionId,
    prcResourceId,
    prcConfiguration,

    -- * Destructuring the Response
    putResourceConfigResponse,
    PutResourceConfigResponse,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putResourceConfig' smart constructor.
data PutResourceConfig = PutResourceConfig'
  { _prcResourceName ::
      !(Maybe Text),
    _prcTags :: !(Maybe (Map Text (Text))),
    _prcResourceType :: !Text,
    _prcSchemaVersionId :: !Text,
    _prcResourceId :: !Text,
    _prcConfiguration :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prcResourceName' - Name of the resource.
--
-- * 'prcTags' - Tags associated with the resource.
--
-- * 'prcResourceType' - The type of the resource. The custom resource type must be registered with AWS CloudFormation.
--
-- * 'prcSchemaVersionId' - Version of the schema registered for the ResourceType in AWS CloudFormation.
--
-- * 'prcResourceId' - Unique identifier of the resource.
--
-- * 'prcConfiguration' - The configuration object of the resource in valid JSON format. It must match the schema registered with AWS CloudFormation.
putResourceConfig ::
  -- | 'prcResourceType'
  Text ->
  -- | 'prcSchemaVersionId'
  Text ->
  -- | 'prcResourceId'
  Text ->
  -- | 'prcConfiguration'
  Text ->
  PutResourceConfig
putResourceConfig
  pResourceType_
  pSchemaVersionId_
  pResourceId_
  pConfiguration_ =
    PutResourceConfig'
      { _prcResourceName = Nothing,
        _prcTags = Nothing,
        _prcResourceType = pResourceType_,
        _prcSchemaVersionId = pSchemaVersionId_,
        _prcResourceId = pResourceId_,
        _prcConfiguration = pConfiguration_
      }

-- | Name of the resource.
prcResourceName :: Lens' PutResourceConfig (Maybe Text)
prcResourceName = lens _prcResourceName (\s a -> s {_prcResourceName = a})

-- | Tags associated with the resource.
prcTags :: Lens' PutResourceConfig (HashMap Text (Text))
prcTags = lens _prcTags (\s a -> s {_prcTags = a}) . _Default . _Map

-- | The type of the resource. The custom resource type must be registered with AWS CloudFormation.
prcResourceType :: Lens' PutResourceConfig Text
prcResourceType = lens _prcResourceType (\s a -> s {_prcResourceType = a})

-- | Version of the schema registered for the ResourceType in AWS CloudFormation.
prcSchemaVersionId :: Lens' PutResourceConfig Text
prcSchemaVersionId = lens _prcSchemaVersionId (\s a -> s {_prcSchemaVersionId = a})

-- | Unique identifier of the resource.
prcResourceId :: Lens' PutResourceConfig Text
prcResourceId = lens _prcResourceId (\s a -> s {_prcResourceId = a})

-- | The configuration object of the resource in valid JSON format. It must match the schema registered with AWS CloudFormation.
prcConfiguration :: Lens' PutResourceConfig Text
prcConfiguration = lens _prcConfiguration (\s a -> s {_prcConfiguration = a})

instance AWSRequest PutResourceConfig where
  type Rs PutResourceConfig = PutResourceConfigResponse
  request = postJSON config
  response = receiveNull PutResourceConfigResponse'

instance Hashable PutResourceConfig

instance NFData PutResourceConfig

instance ToHeaders PutResourceConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.PutResourceConfig" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutResourceConfig where
  toJSON PutResourceConfig' {..} =
    object
      ( catMaybes
          [ ("ResourceName" .=) <$> _prcResourceName,
            ("Tags" .=) <$> _prcTags,
            Just ("ResourceType" .= _prcResourceType),
            Just ("SchemaVersionId" .= _prcSchemaVersionId),
            Just ("ResourceId" .= _prcResourceId),
            Just ("Configuration" .= _prcConfiguration)
          ]
      )

instance ToPath PutResourceConfig where
  toPath = const "/"

instance ToQuery PutResourceConfig where
  toQuery = const mempty

-- | /See:/ 'putResourceConfigResponse' smart constructor.
data PutResourceConfigResponse = PutResourceConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutResourceConfigResponse' with the minimum fields required to make a request.
putResourceConfigResponse ::
  PutResourceConfigResponse
putResourceConfigResponse = PutResourceConfigResponse'

instance NFData PutResourceConfigResponse
