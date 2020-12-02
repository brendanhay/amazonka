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
-- Module      : Network.AWS.Config.BatchGetAggregateResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current configuration items for resources that are present in your AWS Config aggregator. The operation also returns a list of resources that are not processed in the current request. If there are no unprocessed resources, the operation returns an empty @unprocessedResourceIdentifiers@ list.
module Network.AWS.Config.BatchGetAggregateResourceConfig
  ( -- * Creating a Request
    batchGetAggregateResourceConfig,
    BatchGetAggregateResourceConfig,

    -- * Request Lenses
    bgarcConfigurationAggregatorName,
    bgarcResourceIdentifiers,

    -- * Destructuring the Response
    batchGetAggregateResourceConfigResponse,
    BatchGetAggregateResourceConfigResponse,

    -- * Response Lenses
    bgarcrsBaseConfigurationItems,
    bgarcrsUnprocessedResourceIdentifiers,
    bgarcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetAggregateResourceConfig' smart constructor.
data BatchGetAggregateResourceConfig = BatchGetAggregateResourceConfig'
  { _bgarcConfigurationAggregatorName ::
      !Text,
    _bgarcResourceIdentifiers ::
      !( List1
           AggregateResourceIdentifier
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetAggregateResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgarcConfigurationAggregatorName' - The name of the configuration aggregator.
--
-- * 'bgarcResourceIdentifiers' - A list of aggregate ResourceIdentifiers objects.
batchGetAggregateResourceConfig ::
  -- | 'bgarcConfigurationAggregatorName'
  Text ->
  -- | 'bgarcResourceIdentifiers'
  NonEmpty AggregateResourceIdentifier ->
  BatchGetAggregateResourceConfig
batchGetAggregateResourceConfig
  pConfigurationAggregatorName_
  pResourceIdentifiers_ =
    BatchGetAggregateResourceConfig'
      { _bgarcConfigurationAggregatorName =
          pConfigurationAggregatorName_,
        _bgarcResourceIdentifiers = _List1 # pResourceIdentifiers_
      }

-- | The name of the configuration aggregator.
bgarcConfigurationAggregatorName :: Lens' BatchGetAggregateResourceConfig Text
bgarcConfigurationAggregatorName = lens _bgarcConfigurationAggregatorName (\s a -> s {_bgarcConfigurationAggregatorName = a})

-- | A list of aggregate ResourceIdentifiers objects.
bgarcResourceIdentifiers :: Lens' BatchGetAggregateResourceConfig (NonEmpty AggregateResourceIdentifier)
bgarcResourceIdentifiers = lens _bgarcResourceIdentifiers (\s a -> s {_bgarcResourceIdentifiers = a}) . _List1

instance AWSRequest BatchGetAggregateResourceConfig where
  type
    Rs BatchGetAggregateResourceConfig =
      BatchGetAggregateResourceConfigResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          BatchGetAggregateResourceConfigResponse'
            <$> (x .?> "BaseConfigurationItems" .!@ mempty)
            <*> (x .?> "UnprocessedResourceIdentifiers" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchGetAggregateResourceConfig

instance NFData BatchGetAggregateResourceConfig

instance ToHeaders BatchGetAggregateResourceConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.BatchGetAggregateResourceConfig" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchGetAggregateResourceConfig where
  toJSON BatchGetAggregateResourceConfig' {..} =
    object
      ( catMaybes
          [ Just
              ( "ConfigurationAggregatorName"
                  .= _bgarcConfigurationAggregatorName
              ),
            Just ("ResourceIdentifiers" .= _bgarcResourceIdentifiers)
          ]
      )

instance ToPath BatchGetAggregateResourceConfig where
  toPath = const "/"

instance ToQuery BatchGetAggregateResourceConfig where
  toQuery = const mempty

-- | /See:/ 'batchGetAggregateResourceConfigResponse' smart constructor.
data BatchGetAggregateResourceConfigResponse = BatchGetAggregateResourceConfigResponse'
  { _bgarcrsBaseConfigurationItems ::
      !( Maybe
           [BaseConfigurationItem]
       ),
    _bgarcrsUnprocessedResourceIdentifiers ::
      !( Maybe
           [AggregateResourceIdentifier]
       ),
    _bgarcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetAggregateResourceConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgarcrsBaseConfigurationItems' - A list that contains the current configuration of one or more resources.
--
-- * 'bgarcrsUnprocessedResourceIdentifiers' - A list of resource identifiers that were not processed with current scope. The list is empty if all the resources are processed.
--
-- * 'bgarcrsResponseStatus' - -- | The response status code.
batchGetAggregateResourceConfigResponse ::
  -- | 'bgarcrsResponseStatus'
  Int ->
  BatchGetAggregateResourceConfigResponse
batchGetAggregateResourceConfigResponse pResponseStatus_ =
  BatchGetAggregateResourceConfigResponse'
    { _bgarcrsBaseConfigurationItems =
        Nothing,
      _bgarcrsUnprocessedResourceIdentifiers = Nothing,
      _bgarcrsResponseStatus = pResponseStatus_
    }

-- | A list that contains the current configuration of one or more resources.
bgarcrsBaseConfigurationItems :: Lens' BatchGetAggregateResourceConfigResponse [BaseConfigurationItem]
bgarcrsBaseConfigurationItems = lens _bgarcrsBaseConfigurationItems (\s a -> s {_bgarcrsBaseConfigurationItems = a}) . _Default . _Coerce

-- | A list of resource identifiers that were not processed with current scope. The list is empty if all the resources are processed.
bgarcrsUnprocessedResourceIdentifiers :: Lens' BatchGetAggregateResourceConfigResponse [AggregateResourceIdentifier]
bgarcrsUnprocessedResourceIdentifiers = lens _bgarcrsUnprocessedResourceIdentifiers (\s a -> s {_bgarcrsUnprocessedResourceIdentifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
bgarcrsResponseStatus :: Lens' BatchGetAggregateResourceConfigResponse Int
bgarcrsResponseStatus = lens _bgarcrsResponseStatus (\s a -> s {_bgarcrsResponseStatus = a})

instance NFData BatchGetAggregateResourceConfigResponse
