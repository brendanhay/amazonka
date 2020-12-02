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
-- Module      : Network.AWS.Pinpoint.DeleteRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.DeleteRecommenderConfiguration
  ( -- * Creating a Request
    deleteRecommenderConfiguration,
    DeleteRecommenderConfiguration,

    -- * Request Lenses
    drcRecommenderId,

    -- * Destructuring the Response
    deleteRecommenderConfigurationResponse,
    DeleteRecommenderConfigurationResponse,

    -- * Response Lenses
    drcrsResponseStatus,
    drcrsRecommenderConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRecommenderConfiguration' smart constructor.
newtype DeleteRecommenderConfiguration = DeleteRecommenderConfiguration'
  { _drcRecommenderId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRecommenderConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcRecommenderId' - The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
deleteRecommenderConfiguration ::
  -- | 'drcRecommenderId'
  Text ->
  DeleteRecommenderConfiguration
deleteRecommenderConfiguration pRecommenderId_ =
  DeleteRecommenderConfiguration'
    { _drcRecommenderId =
        pRecommenderId_
    }

-- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
drcRecommenderId :: Lens' DeleteRecommenderConfiguration Text
drcRecommenderId = lens _drcRecommenderId (\s a -> s {_drcRecommenderId = a})

instance AWSRequest DeleteRecommenderConfiguration where
  type
    Rs DeleteRecommenderConfiguration =
      DeleteRecommenderConfigurationResponse
  request = delete pinpoint
  response =
    receiveJSON
      ( \s h x ->
          DeleteRecommenderConfigurationResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable DeleteRecommenderConfiguration

instance NFData DeleteRecommenderConfiguration

instance ToHeaders DeleteRecommenderConfiguration where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteRecommenderConfiguration where
  toPath DeleteRecommenderConfiguration' {..} =
    mconcat ["/v1/recommenders/", toBS _drcRecommenderId]

instance ToQuery DeleteRecommenderConfiguration where
  toQuery = const mempty

-- | /See:/ 'deleteRecommenderConfigurationResponse' smart constructor.
data DeleteRecommenderConfigurationResponse = DeleteRecommenderConfigurationResponse'
  { _drcrsResponseStatus ::
      !Int,
    _drcrsRecommenderConfigurationResponse ::
      !RecommenderConfigurationResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRecommenderConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcrsResponseStatus' - -- | The response status code.
--
-- * 'drcrsRecommenderConfigurationResponse' - Undocumented member.
deleteRecommenderConfigurationResponse ::
  -- | 'drcrsResponseStatus'
  Int ->
  -- | 'drcrsRecommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  DeleteRecommenderConfigurationResponse
deleteRecommenderConfigurationResponse
  pResponseStatus_
  pRecommenderConfigurationResponse_ =
    DeleteRecommenderConfigurationResponse'
      { _drcrsResponseStatus =
          pResponseStatus_,
        _drcrsRecommenderConfigurationResponse =
          pRecommenderConfigurationResponse_
      }

-- | -- | The response status code.
drcrsResponseStatus :: Lens' DeleteRecommenderConfigurationResponse Int
drcrsResponseStatus = lens _drcrsResponseStatus (\s a -> s {_drcrsResponseStatus = a})

-- | Undocumented member.
drcrsRecommenderConfigurationResponse :: Lens' DeleteRecommenderConfigurationResponse RecommenderConfigurationResponse
drcrsRecommenderConfigurationResponse = lens _drcrsRecommenderConfigurationResponse (\s a -> s {_drcrsRecommenderConfigurationResponse = a})

instance NFData DeleteRecommenderConfigurationResponse
