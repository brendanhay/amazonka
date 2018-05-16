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
-- Module      : Network.AWS.Lightsail.UpdateLoadBalancerAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attribute for a load balancer. You can only update one attribute at a time.
--
--
module Network.AWS.Lightsail.UpdateLoadBalancerAttribute
    (
    -- * Creating a Request
      updateLoadBalancerAttribute
    , UpdateLoadBalancerAttribute
    -- * Request Lenses
    , ulbaLoadBalancerName
    , ulbaAttributeName
    , ulbaAttributeValue

    -- * Destructuring the Response
    , updateLoadBalancerAttributeResponse
    , UpdateLoadBalancerAttributeResponse
    -- * Response Lenses
    , ulbarsOperations
    , ulbarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateLoadBalancerAttribute' smart constructor.
data UpdateLoadBalancerAttribute = UpdateLoadBalancerAttribute'
  { _ulbaLoadBalancerName :: !Text
  , _ulbaAttributeName    :: !LoadBalancerAttributeName
  , _ulbaAttributeValue   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLoadBalancerAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulbaLoadBalancerName' - The name of the load balancer that you want to modify (e.g., @my-load-balancer@ .
--
-- * 'ulbaAttributeName' - The name of the attribute you want to update. Valid values are below.
--
-- * 'ulbaAttributeValue' - The value that you want to specify for the attribute name.
updateLoadBalancerAttribute
    :: Text -- ^ 'ulbaLoadBalancerName'
    -> LoadBalancerAttributeName -- ^ 'ulbaAttributeName'
    -> Text -- ^ 'ulbaAttributeValue'
    -> UpdateLoadBalancerAttribute
updateLoadBalancerAttribute pLoadBalancerName_ pAttributeName_ pAttributeValue_ =
  UpdateLoadBalancerAttribute'
    { _ulbaLoadBalancerName = pLoadBalancerName_
    , _ulbaAttributeName = pAttributeName_
    , _ulbaAttributeValue = pAttributeValue_
    }


-- | The name of the load balancer that you want to modify (e.g., @my-load-balancer@ .
ulbaLoadBalancerName :: Lens' UpdateLoadBalancerAttribute Text
ulbaLoadBalancerName = lens _ulbaLoadBalancerName (\ s a -> s{_ulbaLoadBalancerName = a})

-- | The name of the attribute you want to update. Valid values are below.
ulbaAttributeName :: Lens' UpdateLoadBalancerAttribute LoadBalancerAttributeName
ulbaAttributeName = lens _ulbaAttributeName (\ s a -> s{_ulbaAttributeName = a})

-- | The value that you want to specify for the attribute name.
ulbaAttributeValue :: Lens' UpdateLoadBalancerAttribute Text
ulbaAttributeValue = lens _ulbaAttributeValue (\ s a -> s{_ulbaAttributeValue = a})

instance AWSRequest UpdateLoadBalancerAttribute where
        type Rs UpdateLoadBalancerAttribute =
             UpdateLoadBalancerAttributeResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 UpdateLoadBalancerAttributeResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable UpdateLoadBalancerAttribute where

instance NFData UpdateLoadBalancerAttribute where

instance ToHeaders UpdateLoadBalancerAttribute where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.UpdateLoadBalancerAttribute" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateLoadBalancerAttribute where
        toJSON UpdateLoadBalancerAttribute'{..}
          = object
              (catMaybes
                 [Just ("loadBalancerName" .= _ulbaLoadBalancerName),
                  Just ("attributeName" .= _ulbaAttributeName),
                  Just ("attributeValue" .= _ulbaAttributeValue)])

instance ToPath UpdateLoadBalancerAttribute where
        toPath = const "/"

instance ToQuery UpdateLoadBalancerAttribute where
        toQuery = const mempty

-- | /See:/ 'updateLoadBalancerAttributeResponse' smart constructor.
data UpdateLoadBalancerAttributeResponse = UpdateLoadBalancerAttributeResponse'
  { _ulbarsOperations     :: !(Maybe [Operation])
  , _ulbarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLoadBalancerAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulbarsOperations' - An object describing the API operations.
--
-- * 'ulbarsResponseStatus' - -- | The response status code.
updateLoadBalancerAttributeResponse
    :: Int -- ^ 'ulbarsResponseStatus'
    -> UpdateLoadBalancerAttributeResponse
updateLoadBalancerAttributeResponse pResponseStatus_ =
  UpdateLoadBalancerAttributeResponse'
    {_ulbarsOperations = Nothing, _ulbarsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
ulbarsOperations :: Lens' UpdateLoadBalancerAttributeResponse [Operation]
ulbarsOperations = lens _ulbarsOperations (\ s a -> s{_ulbarsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
ulbarsResponseStatus :: Lens' UpdateLoadBalancerAttributeResponse Int
ulbarsResponseStatus = lens _ulbarsResponseStatus (\ s a -> s{_ulbarsResponseStatus = a})

instance NFData UpdateLoadBalancerAttributeResponse
         where
