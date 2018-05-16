{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates variant weight of one or more variants associated with an existing endpoint, or capacity of one variant associated with an existing endpoint. When it receives the request, Amazon SageMaker sets the endpoint status to @Updating@ . After updating the endpoint, it sets the status to @InService@ . To check the status of an endpoint, use the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_DescribeEndpoint.html DescribeEndpoint> API.
--
--
module Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
    (
    -- * Creating a Request
      updateEndpointWeightsAndCapacities
    , UpdateEndpointWeightsAndCapacities
    -- * Request Lenses
    , uewacEndpointName
    , uewacDesiredWeightsAndCapacities

    -- * Destructuring the Response
    , updateEndpointWeightsAndCapacitiesResponse
    , UpdateEndpointWeightsAndCapacitiesResponse
    -- * Response Lenses
    , uewacrsResponseStatus
    , uewacrsEndpointARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'updateEndpointWeightsAndCapacities' smart constructor.
data UpdateEndpointWeightsAndCapacities = UpdateEndpointWeightsAndCapacities'
  { _uewacEndpointName                :: !Text
  , _uewacDesiredWeightsAndCapacities :: !(List1 DesiredWeightAndCapacity)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEndpointWeightsAndCapacities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uewacEndpointName' - The name of an existing Amazon SageMaker endpoint.
--
-- * 'uewacDesiredWeightsAndCapacities' - An object that provides new capacity and weight values for a variant.
updateEndpointWeightsAndCapacities
    :: Text -- ^ 'uewacEndpointName'
    -> NonEmpty DesiredWeightAndCapacity -- ^ 'uewacDesiredWeightsAndCapacities'
    -> UpdateEndpointWeightsAndCapacities
updateEndpointWeightsAndCapacities pEndpointName_ pDesiredWeightsAndCapacities_ =
  UpdateEndpointWeightsAndCapacities'
    { _uewacEndpointName = pEndpointName_
    , _uewacDesiredWeightsAndCapacities = _List1 # pDesiredWeightsAndCapacities_
    }


-- | The name of an existing Amazon SageMaker endpoint.
uewacEndpointName :: Lens' UpdateEndpointWeightsAndCapacities Text
uewacEndpointName = lens _uewacEndpointName (\ s a -> s{_uewacEndpointName = a})

-- | An object that provides new capacity and weight values for a variant.
uewacDesiredWeightsAndCapacities :: Lens' UpdateEndpointWeightsAndCapacities (NonEmpty DesiredWeightAndCapacity)
uewacDesiredWeightsAndCapacities = lens _uewacDesiredWeightsAndCapacities (\ s a -> s{_uewacDesiredWeightsAndCapacities = a}) . _List1

instance AWSRequest
           UpdateEndpointWeightsAndCapacities
         where
        type Rs UpdateEndpointWeightsAndCapacities =
             UpdateEndpointWeightsAndCapacitiesResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 UpdateEndpointWeightsAndCapacitiesResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "EndpointArn"))

instance Hashable UpdateEndpointWeightsAndCapacities
         where

instance NFData UpdateEndpointWeightsAndCapacities
         where

instance ToHeaders UpdateEndpointWeightsAndCapacities
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.UpdateEndpointWeightsAndCapacities" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateEndpointWeightsAndCapacities
         where
        toJSON UpdateEndpointWeightsAndCapacities'{..}
          = object
              (catMaybes
                 [Just ("EndpointName" .= _uewacEndpointName),
                  Just
                    ("DesiredWeightsAndCapacities" .=
                       _uewacDesiredWeightsAndCapacities)])

instance ToPath UpdateEndpointWeightsAndCapacities
         where
        toPath = const "/"

instance ToQuery UpdateEndpointWeightsAndCapacities
         where
        toQuery = const mempty

-- | /See:/ 'updateEndpointWeightsAndCapacitiesResponse' smart constructor.
data UpdateEndpointWeightsAndCapacitiesResponse = UpdateEndpointWeightsAndCapacitiesResponse'
  { _uewacrsResponseStatus :: !Int
  , _uewacrsEndpointARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEndpointWeightsAndCapacitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uewacrsResponseStatus' - -- | The response status code.
--
-- * 'uewacrsEndpointARN' - The Amazon Resource Name (ARN) of the updated endpoint.
updateEndpointWeightsAndCapacitiesResponse
    :: Int -- ^ 'uewacrsResponseStatus'
    -> Text -- ^ 'uewacrsEndpointARN'
    -> UpdateEndpointWeightsAndCapacitiesResponse
updateEndpointWeightsAndCapacitiesResponse pResponseStatus_ pEndpointARN_ =
  UpdateEndpointWeightsAndCapacitiesResponse'
    { _uewacrsResponseStatus = pResponseStatus_
    , _uewacrsEndpointARN = pEndpointARN_
    }


-- | -- | The response status code.
uewacrsResponseStatus :: Lens' UpdateEndpointWeightsAndCapacitiesResponse Int
uewacrsResponseStatus = lens _uewacrsResponseStatus (\ s a -> s{_uewacrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the updated endpoint.
uewacrsEndpointARN :: Lens' UpdateEndpointWeightsAndCapacitiesResponse Text
uewacrsEndpointARN = lens _uewacrsEndpointARN (\ s a -> s{_uewacrsEndpointARN = a})

instance NFData
           UpdateEndpointWeightsAndCapacitiesResponse
         where
