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
-- Module      : Network.AWS.Greengrass.DeleteSubscriptionDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription definition.
module Network.AWS.Greengrass.DeleteSubscriptionDefinition
    (
    -- * Creating a Request
      deleteSubscriptionDefinition
    , DeleteSubscriptionDefinition
    -- * Request Lenses
    , dsdSubscriptionDefinitionId

    -- * Destructuring the Response
    , deleteSubscriptionDefinitionResponse
    , DeleteSubscriptionDefinitionResponse
    -- * Response Lenses
    , dsdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSubscriptionDefinition' smart constructor.
newtype DeleteSubscriptionDefinition = DeleteSubscriptionDefinition'
  { _dsdSubscriptionDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubscriptionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdSubscriptionDefinitionId' - The ID of the subscription definition.
deleteSubscriptionDefinition
    :: Text -- ^ 'dsdSubscriptionDefinitionId'
    -> DeleteSubscriptionDefinition
deleteSubscriptionDefinition pSubscriptionDefinitionId_ =
  DeleteSubscriptionDefinition'
    {_dsdSubscriptionDefinitionId = pSubscriptionDefinitionId_}


-- | The ID of the subscription definition.
dsdSubscriptionDefinitionId :: Lens' DeleteSubscriptionDefinition Text
dsdSubscriptionDefinitionId = lens _dsdSubscriptionDefinitionId (\ s a -> s{_dsdSubscriptionDefinitionId = a})

instance AWSRequest DeleteSubscriptionDefinition
         where
        type Rs DeleteSubscriptionDefinition =
             DeleteSubscriptionDefinitionResponse
        request = delete greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteSubscriptionDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteSubscriptionDefinition where

instance NFData DeleteSubscriptionDefinition where

instance ToHeaders DeleteSubscriptionDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteSubscriptionDefinition where
        toPath DeleteSubscriptionDefinition'{..}
          = mconcat
              ["/greengrass/definition/subscriptions/",
               toBS _dsdSubscriptionDefinitionId]

instance ToQuery DeleteSubscriptionDefinition where
        toQuery = const mempty

-- | /See:/ 'deleteSubscriptionDefinitionResponse' smart constructor.
newtype DeleteSubscriptionDefinitionResponse = DeleteSubscriptionDefinitionResponse'
  { _dsdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubscriptionDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdrsResponseStatus' - -- | The response status code.
deleteSubscriptionDefinitionResponse
    :: Int -- ^ 'dsdrsResponseStatus'
    -> DeleteSubscriptionDefinitionResponse
deleteSubscriptionDefinitionResponse pResponseStatus_ =
  DeleteSubscriptionDefinitionResponse'
    {_dsdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsdrsResponseStatus :: Lens' DeleteSubscriptionDefinitionResponse Int
dsdrsResponseStatus = lens _dsdrsResponseStatus (\ s a -> s{_dsdrsResponseStatus = a})

instance NFData DeleteSubscriptionDefinitionResponse
         where
