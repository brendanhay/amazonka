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
-- Module      : Network.AWS.Greengrass.UpdateSubscriptionDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscription definition.
module Network.AWS.Greengrass.UpdateSubscriptionDefinition
    (
    -- * Creating a Request
      updateSubscriptionDefinition
    , UpdateSubscriptionDefinition
    -- * Request Lenses
    , usdName
    , usdSubscriptionDefinitionId

    -- * Destructuring the Response
    , updateSubscriptionDefinitionResponse
    , UpdateSubscriptionDefinitionResponse
    -- * Response Lenses
    , usdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSubscriptionDefinition' smart constructor.
data UpdateSubscriptionDefinition = UpdateSubscriptionDefinition'
  { _usdName                     :: !(Maybe Text)
  , _usdSubscriptionDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSubscriptionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usdName' - The name of the definition.
--
-- * 'usdSubscriptionDefinitionId' - The ID of the subscription definition.
updateSubscriptionDefinition
    :: Text -- ^ 'usdSubscriptionDefinitionId'
    -> UpdateSubscriptionDefinition
updateSubscriptionDefinition pSubscriptionDefinitionId_ =
  UpdateSubscriptionDefinition'
    { _usdName = Nothing
    , _usdSubscriptionDefinitionId = pSubscriptionDefinitionId_
    }


-- | The name of the definition.
usdName :: Lens' UpdateSubscriptionDefinition (Maybe Text)
usdName = lens _usdName (\ s a -> s{_usdName = a})

-- | The ID of the subscription definition.
usdSubscriptionDefinitionId :: Lens' UpdateSubscriptionDefinition Text
usdSubscriptionDefinitionId = lens _usdSubscriptionDefinitionId (\ s a -> s{_usdSubscriptionDefinitionId = a})

instance AWSRequest UpdateSubscriptionDefinition
         where
        type Rs UpdateSubscriptionDefinition =
             UpdateSubscriptionDefinitionResponse
        request = putJSON greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateSubscriptionDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateSubscriptionDefinition where

instance NFData UpdateSubscriptionDefinition where

instance ToHeaders UpdateSubscriptionDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSubscriptionDefinition where
        toJSON UpdateSubscriptionDefinition'{..}
          = object (catMaybes [("Name" .=) <$> _usdName])

instance ToPath UpdateSubscriptionDefinition where
        toPath UpdateSubscriptionDefinition'{..}
          = mconcat
              ["/greengrass/definition/subscriptions/",
               toBS _usdSubscriptionDefinitionId]

instance ToQuery UpdateSubscriptionDefinition where
        toQuery = const mempty

-- | /See:/ 'updateSubscriptionDefinitionResponse' smart constructor.
newtype UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse'
  { _usdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSubscriptionDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usdrsResponseStatus' - -- | The response status code.
updateSubscriptionDefinitionResponse
    :: Int -- ^ 'usdrsResponseStatus'
    -> UpdateSubscriptionDefinitionResponse
updateSubscriptionDefinitionResponse pResponseStatus_ =
  UpdateSubscriptionDefinitionResponse'
    {_usdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
usdrsResponseStatus :: Lens' UpdateSubscriptionDefinitionResponse Int
usdrsResponseStatus = lens _usdrsResponseStatus (\ s a -> s{_usdrsResponseStatus = a})

instance NFData UpdateSubscriptionDefinitionResponse
         where
