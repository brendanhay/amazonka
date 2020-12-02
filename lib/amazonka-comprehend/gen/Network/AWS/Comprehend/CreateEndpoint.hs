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
-- Module      : Network.AWS.Comprehend.CreateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model-specific endpoint for synchronous inference for a previously trained custom model
module Network.AWS.Comprehend.CreateEndpoint
  ( -- * Creating a Request
    createEndpoint,
    CreateEndpoint,

    -- * Request Lenses
    ceClientRequestToken,
    ceTags,
    ceEndpointName,
    ceModelARN,
    ceDesiredInferenceUnits,

    -- * Destructuring the Response
    createEndpointResponse,
    CreateEndpointResponse,

    -- * Response Lenses
    cersEndpointARN,
    cersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { _ceClientRequestToken ::
      !(Maybe Text),
    _ceTags :: !(Maybe [Tag]),
    _ceEndpointName :: !Text,
    _ceModelARN :: !Text,
    _ceDesiredInferenceUnits :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceClientRequestToken' - An idempotency token provided by the customer. If this token matches a previous endpoint creation request, Amazon Comprehend will not return a @ResourceInUseException@ .
--
-- * 'ceTags' - Tags associated with the endpoint being created. A tag is a key-value pair that adds metadata to the endpoint. For example, a tag with "Sales" as the key might be added to an endpoint to indicate its use by the sales department.
--
-- * 'ceEndpointName' - This is the descriptive suffix that becomes part of the @EndpointArn@ used for all subsequent requests to this resource.
--
-- * 'ceModelARN' - The Amazon Resource Number (ARN) of the model to which the endpoint will be attached.
--
-- * 'ceDesiredInferenceUnits' - The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
createEndpoint ::
  -- | 'ceEndpointName'
  Text ->
  -- | 'ceModelARN'
  Text ->
  -- | 'ceDesiredInferenceUnits'
  Natural ->
  CreateEndpoint
createEndpoint pEndpointName_ pModelARN_ pDesiredInferenceUnits_ =
  CreateEndpoint'
    { _ceClientRequestToken = Nothing,
      _ceTags = Nothing,
      _ceEndpointName = pEndpointName_,
      _ceModelARN = pModelARN_,
      _ceDesiredInferenceUnits = _Nat # pDesiredInferenceUnits_
    }

-- | An idempotency token provided by the customer. If this token matches a previous endpoint creation request, Amazon Comprehend will not return a @ResourceInUseException@ .
ceClientRequestToken :: Lens' CreateEndpoint (Maybe Text)
ceClientRequestToken = lens _ceClientRequestToken (\s a -> s {_ceClientRequestToken = a})

-- | Tags associated with the endpoint being created. A tag is a key-value pair that adds metadata to the endpoint. For example, a tag with "Sales" as the key might be added to an endpoint to indicate its use by the sales department.
ceTags :: Lens' CreateEndpoint [Tag]
ceTags = lens _ceTags (\s a -> s {_ceTags = a}) . _Default . _Coerce

-- | This is the descriptive suffix that becomes part of the @EndpointArn@ used for all subsequent requests to this resource.
ceEndpointName :: Lens' CreateEndpoint Text
ceEndpointName = lens _ceEndpointName (\s a -> s {_ceEndpointName = a})

-- | The Amazon Resource Number (ARN) of the model to which the endpoint will be attached.
ceModelARN :: Lens' CreateEndpoint Text
ceModelARN = lens _ceModelARN (\s a -> s {_ceModelARN = a})

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
ceDesiredInferenceUnits :: Lens' CreateEndpoint Natural
ceDesiredInferenceUnits = lens _ceDesiredInferenceUnits (\s a -> s {_ceDesiredInferenceUnits = a}) . _Nat

instance AWSRequest CreateEndpoint where
  type Rs CreateEndpoint = CreateEndpointResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            <$> (x .?> "EndpointArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateEndpoint

instance NFData CreateEndpoint

instance ToHeaders CreateEndpoint where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.CreateEndpoint" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    object
      ( catMaybes
          [ ("ClientRequestToken" .=) <$> _ceClientRequestToken,
            ("Tags" .=) <$> _ceTags,
            Just ("EndpointName" .= _ceEndpointName),
            Just ("ModelArn" .= _ceModelARN),
            Just ("DesiredInferenceUnits" .= _ceDesiredInferenceUnits)
          ]
      )

instance ToPath CreateEndpoint where
  toPath = const "/"

instance ToQuery CreateEndpoint where
  toQuery = const mempty

-- | /See:/ 'createEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { _cersEndpointARN ::
      !(Maybe Text),
    _cersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cersEndpointARN' - The Amazon Resource Number (ARN) of the endpoint being created.
--
-- * 'cersResponseStatus' - -- | The response status code.
createEndpointResponse ::
  -- | 'cersResponseStatus'
  Int ->
  CreateEndpointResponse
createEndpointResponse pResponseStatus_ =
  CreateEndpointResponse'
    { _cersEndpointARN = Nothing,
      _cersResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Number (ARN) of the endpoint being created.
cersEndpointARN :: Lens' CreateEndpointResponse (Maybe Text)
cersEndpointARN = lens _cersEndpointARN (\s a -> s {_cersEndpointARN = a})

-- | -- | The response status code.
cersResponseStatus :: Lens' CreateEndpointResponse Int
cersResponseStatus = lens _cersResponseStatus (\s a -> s {_cersResponseStatus = a})

instance NFData CreateEndpointResponse
