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
-- Module      : Network.AWS.Glue.BatchGetDevEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of development endpoint names. After calling the @ListDevEndpoints@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetDevEndpoints
  ( -- * Creating a Request
    batchGetDevEndpoints,
    BatchGetDevEndpoints,

    -- * Request Lenses
    bgdeDevEndpointNames,

    -- * Destructuring the Response
    batchGetDevEndpointsResponse,
    BatchGetDevEndpointsResponse,

    -- * Response Lenses
    bgdersDevEndpointsNotFound,
    bgdersDevEndpoints,
    bgdersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetDevEndpoints' smart constructor.
newtype BatchGetDevEndpoints = BatchGetDevEndpoints'
  { _bgdeDevEndpointNames ::
      List1 Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetDevEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdeDevEndpointNames' - The list of @DevEndpoint@ names, which might be the names returned from the @ListDevEndpoint@ operation.
batchGetDevEndpoints ::
  -- | 'bgdeDevEndpointNames'
  NonEmpty Text ->
  BatchGetDevEndpoints
batchGetDevEndpoints pDevEndpointNames_ =
  BatchGetDevEndpoints'
    { _bgdeDevEndpointNames =
        _List1 # pDevEndpointNames_
    }

-- | The list of @DevEndpoint@ names, which might be the names returned from the @ListDevEndpoint@ operation.
bgdeDevEndpointNames :: Lens' BatchGetDevEndpoints (NonEmpty Text)
bgdeDevEndpointNames = lens _bgdeDevEndpointNames (\s a -> s {_bgdeDevEndpointNames = a}) . _List1

instance AWSRequest BatchGetDevEndpoints where
  type Rs BatchGetDevEndpoints = BatchGetDevEndpointsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          BatchGetDevEndpointsResponse'
            <$> (x .?> "DevEndpointsNotFound")
            <*> (x .?> "DevEndpoints" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchGetDevEndpoints

instance NFData BatchGetDevEndpoints

instance ToHeaders BatchGetDevEndpoints where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.BatchGetDevEndpoints" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchGetDevEndpoints where
  toJSON BatchGetDevEndpoints' {..} =
    object
      (catMaybes [Just ("DevEndpointNames" .= _bgdeDevEndpointNames)])

instance ToPath BatchGetDevEndpoints where
  toPath = const "/"

instance ToQuery BatchGetDevEndpoints where
  toQuery = const mempty

-- | /See:/ 'batchGetDevEndpointsResponse' smart constructor.
data BatchGetDevEndpointsResponse = BatchGetDevEndpointsResponse'
  { _bgdersDevEndpointsNotFound ::
      !(Maybe (List1 Text)),
    _bgdersDevEndpoints ::
      !(Maybe [DevEndpoint]),
    _bgdersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetDevEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdersDevEndpointsNotFound' - A list of @DevEndpoints@ not found.
--
-- * 'bgdersDevEndpoints' - A list of @DevEndpoint@ definitions.
--
-- * 'bgdersResponseStatus' - -- | The response status code.
batchGetDevEndpointsResponse ::
  -- | 'bgdersResponseStatus'
  Int ->
  BatchGetDevEndpointsResponse
batchGetDevEndpointsResponse pResponseStatus_ =
  BatchGetDevEndpointsResponse'
    { _bgdersDevEndpointsNotFound =
        Nothing,
      _bgdersDevEndpoints = Nothing,
      _bgdersResponseStatus = pResponseStatus_
    }

-- | A list of @DevEndpoints@ not found.
bgdersDevEndpointsNotFound :: Lens' BatchGetDevEndpointsResponse (Maybe (NonEmpty Text))
bgdersDevEndpointsNotFound = lens _bgdersDevEndpointsNotFound (\s a -> s {_bgdersDevEndpointsNotFound = a}) . mapping _List1

-- | A list of @DevEndpoint@ definitions.
bgdersDevEndpoints :: Lens' BatchGetDevEndpointsResponse [DevEndpoint]
bgdersDevEndpoints = lens _bgdersDevEndpoints (\s a -> s {_bgdersDevEndpoints = a}) . _Default . _Coerce

-- | -- | The response status code.
bgdersResponseStatus :: Lens' BatchGetDevEndpointsResponse Int
bgdersResponseStatus = lens _bgdersResponseStatus (\s a -> s {_bgdersResponseStatus = a})

instance NFData BatchGetDevEndpointsResponse
