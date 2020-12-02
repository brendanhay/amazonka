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
-- Module      : Network.AWS.ServiceCatalog.CreateServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a self-service action.
module Network.AWS.ServiceCatalog.CreateServiceAction
  ( -- * Creating a Request
    createServiceAction,
    CreateServiceAction,

    -- * Request Lenses
    csaAcceptLanguage,
    csaDescription,
    csaName,
    csaDefinitionType,
    csaDefinition,
    csaIdempotencyToken,

    -- * Destructuring the Response
    createServiceActionResponse,
    CreateServiceActionResponse,

    -- * Response Lenses
    csarsServiceActionDetail,
    csarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'createServiceAction' smart constructor.
data CreateServiceAction = CreateServiceAction'
  { _csaAcceptLanguage ::
      !(Maybe Text),
    _csaDescription :: !(Maybe Text),
    _csaName :: !Text,
    _csaDefinitionType :: !ServiceActionDefinitionType,
    _csaDefinition ::
      !(Map ServiceActionDefinitionKey (Text)),
    _csaIdempotencyToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateServiceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'csaDescription' - The self-service action description.
--
-- * 'csaName' - The self-service action name.
--
-- * 'csaDefinitionType' - The service action definition type. For example, @SSM_AUTOMATION@ .
--
-- * 'csaDefinition' - The self-service action definition. Can be one of the following:     * Name    * The name of the AWS Systems Manager document (SSM document). For example, @AWS-RestartEC2Instance@ . If you are using a shared SSM document, you must provide the ARN instead of the name.     * Version    * The AWS Systems Manager automation document version. For example, @"Version": "1"@      * AssumeRole    * The Amazon Resource Name (ARN) of the role that performs the self-service actions on your behalf. For example, @"AssumeRole": "arn:aws:iam::12345678910:role/ActionRole"@ . To reuse the provisioned product launch role, set to @"AssumeRole": "LAUNCH_ROLE"@ .     * Parameters    * The list of parameters in JSON format. For example: @[{\"Name\":\"InstanceId\",\"Type\":\"TARGET\"}]@ or @[{\"Name\":\"InstanceId\",\"Type\":\"TEXT_VALUE\"}]@ .
--
-- * 'csaIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
createServiceAction ::
  -- | 'csaName'
  Text ->
  -- | 'csaDefinitionType'
  ServiceActionDefinitionType ->
  -- | 'csaIdempotencyToken'
  Text ->
  CreateServiceAction
createServiceAction pName_ pDefinitionType_ pIdempotencyToken_ =
  CreateServiceAction'
    { _csaAcceptLanguage = Nothing,
      _csaDescription = Nothing,
      _csaName = pName_,
      _csaDefinitionType = pDefinitionType_,
      _csaDefinition = mempty,
      _csaIdempotencyToken = pIdempotencyToken_
    }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
csaAcceptLanguage :: Lens' CreateServiceAction (Maybe Text)
csaAcceptLanguage = lens _csaAcceptLanguage (\s a -> s {_csaAcceptLanguage = a})

-- | The self-service action description.
csaDescription :: Lens' CreateServiceAction (Maybe Text)
csaDescription = lens _csaDescription (\s a -> s {_csaDescription = a})

-- | The self-service action name.
csaName :: Lens' CreateServiceAction Text
csaName = lens _csaName (\s a -> s {_csaName = a})

-- | The service action definition type. For example, @SSM_AUTOMATION@ .
csaDefinitionType :: Lens' CreateServiceAction ServiceActionDefinitionType
csaDefinitionType = lens _csaDefinitionType (\s a -> s {_csaDefinitionType = a})

-- | The self-service action definition. Can be one of the following:     * Name    * The name of the AWS Systems Manager document (SSM document). For example, @AWS-RestartEC2Instance@ . If you are using a shared SSM document, you must provide the ARN instead of the name.     * Version    * The AWS Systems Manager automation document version. For example, @"Version": "1"@      * AssumeRole    * The Amazon Resource Name (ARN) of the role that performs the self-service actions on your behalf. For example, @"AssumeRole": "arn:aws:iam::12345678910:role/ActionRole"@ . To reuse the provisioned product launch role, set to @"AssumeRole": "LAUNCH_ROLE"@ .     * Parameters    * The list of parameters in JSON format. For example: @[{\"Name\":\"InstanceId\",\"Type\":\"TARGET\"}]@ or @[{\"Name\":\"InstanceId\",\"Type\":\"TEXT_VALUE\"}]@ .
csaDefinition :: Lens' CreateServiceAction (HashMap ServiceActionDefinitionKey (Text))
csaDefinition = lens _csaDefinition (\s a -> s {_csaDefinition = a}) . _Map

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
csaIdempotencyToken :: Lens' CreateServiceAction Text
csaIdempotencyToken = lens _csaIdempotencyToken (\s a -> s {_csaIdempotencyToken = a})

instance AWSRequest CreateServiceAction where
  type Rs CreateServiceAction = CreateServiceActionResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          CreateServiceActionResponse'
            <$> (x .?> "ServiceActionDetail") <*> (pure (fromEnum s))
      )

instance Hashable CreateServiceAction

instance NFData CreateServiceAction

instance ToHeaders CreateServiceAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.CreateServiceAction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateServiceAction where
  toJSON CreateServiceAction' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _csaAcceptLanguage,
            ("Description" .=) <$> _csaDescription,
            Just ("Name" .= _csaName),
            Just ("DefinitionType" .= _csaDefinitionType),
            Just ("Definition" .= _csaDefinition),
            Just ("IdempotencyToken" .= _csaIdempotencyToken)
          ]
      )

instance ToPath CreateServiceAction where
  toPath = const "/"

instance ToQuery CreateServiceAction where
  toQuery = const mempty

-- | /See:/ 'createServiceActionResponse' smart constructor.
data CreateServiceActionResponse = CreateServiceActionResponse'
  { _csarsServiceActionDetail ::
      !(Maybe ServiceActionDetail),
    _csarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateServiceActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csarsServiceActionDetail' - An object containing information about the self-service action.
--
-- * 'csarsResponseStatus' - -- | The response status code.
createServiceActionResponse ::
  -- | 'csarsResponseStatus'
  Int ->
  CreateServiceActionResponse
createServiceActionResponse pResponseStatus_ =
  CreateServiceActionResponse'
    { _csarsServiceActionDetail = Nothing,
      _csarsResponseStatus = pResponseStatus_
    }

-- | An object containing information about the self-service action.
csarsServiceActionDetail :: Lens' CreateServiceActionResponse (Maybe ServiceActionDetail)
csarsServiceActionDetail = lens _csarsServiceActionDetail (\s a -> s {_csarsServiceActionDetail = a})

-- | -- | The response status code.
csarsResponseStatus :: Lens' CreateServiceActionResponse Int
csarsResponseStatus = lens _csarsResponseStatus (\s a -> s {_csarsResponseStatus = a})

instance NFData CreateServiceActionResponse
