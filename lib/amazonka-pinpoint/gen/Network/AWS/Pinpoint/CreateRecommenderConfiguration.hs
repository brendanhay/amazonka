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
-- Module      : Network.AWS.Pinpoint.CreateRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.CreateRecommenderConfiguration
  ( -- * Creating a Request
    createRecommenderConfiguration,
    CreateRecommenderConfiguration,

    -- * Request Lenses
    crcCreateRecommenderConfiguration,

    -- * Destructuring the Response
    createRecommenderConfigurationResponse,
    CreateRecommenderConfigurationResponse,

    -- * Response Lenses
    crcrsResponseStatus,
    crcrsRecommenderConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRecommenderConfiguration' smart constructor.
newtype CreateRecommenderConfiguration = CreateRecommenderConfiguration'
  { _crcCreateRecommenderConfiguration ::
      CreateRecommenderConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRecommenderConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crcCreateRecommenderConfiguration' - Undocumented member.
createRecommenderConfiguration ::
  -- | 'crcCreateRecommenderConfiguration'
  CreateRecommenderConfiguration ->
  CreateRecommenderConfiguration
createRecommenderConfiguration pCreateRecommenderConfiguration_ =
  CreateRecommenderConfiguration'
    { _crcCreateRecommenderConfiguration =
        pCreateRecommenderConfiguration_
    }

-- | Undocumented member.
crcCreateRecommenderConfiguration :: Lens' CreateRecommenderConfiguration CreateRecommenderConfiguration
crcCreateRecommenderConfiguration = lens _crcCreateRecommenderConfiguration (\s a -> s {_crcCreateRecommenderConfiguration = a})

instance AWSRequest CreateRecommenderConfiguration where
  type
    Rs CreateRecommenderConfiguration =
      CreateRecommenderConfigurationResponse
  request = postJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          CreateRecommenderConfigurationResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable CreateRecommenderConfiguration

instance NFData CreateRecommenderConfiguration

instance ToHeaders CreateRecommenderConfiguration where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateRecommenderConfiguration where
  toJSON CreateRecommenderConfiguration' {..} =
    object
      ( catMaybes
          [ Just
              ( "CreateRecommenderConfiguration"
                  .= _crcCreateRecommenderConfiguration
              )
          ]
      )

instance ToPath CreateRecommenderConfiguration where
  toPath = const "/v1/recommenders"

instance ToQuery CreateRecommenderConfiguration where
  toQuery = const mempty

-- | /See:/ 'createRecommenderConfigurationResponse' smart constructor.
data CreateRecommenderConfigurationResponse = CreateRecommenderConfigurationResponse'
  { _crcrsResponseStatus ::
      !Int,
    _crcrsRecommenderConfigurationResponse ::
      !RecommenderConfigurationResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRecommenderConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crcrsResponseStatus' - -- | The response status code.
--
-- * 'crcrsRecommenderConfigurationResponse' - Undocumented member.
createRecommenderConfigurationResponse ::
  -- | 'crcrsResponseStatus'
  Int ->
  -- | 'crcrsRecommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  CreateRecommenderConfigurationResponse
createRecommenderConfigurationResponse
  pResponseStatus_
  pRecommenderConfigurationResponse_ =
    CreateRecommenderConfigurationResponse'
      { _crcrsResponseStatus =
          pResponseStatus_,
        _crcrsRecommenderConfigurationResponse =
          pRecommenderConfigurationResponse_
      }

-- | -- | The response status code.
crcrsResponseStatus :: Lens' CreateRecommenderConfigurationResponse Int
crcrsResponseStatus = lens _crcrsResponseStatus (\s a -> s {_crcrsResponseStatus = a})

-- | Undocumented member.
crcrsRecommenderConfigurationResponse :: Lens' CreateRecommenderConfigurationResponse RecommenderConfigurationResponse
crcrsRecommenderConfigurationResponse = lens _crcrsRecommenderConfigurationResponse (\s a -> s {_crcrsRecommenderConfigurationResponse = a})

instance NFData CreateRecommenderConfigurationResponse
