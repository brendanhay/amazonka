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
-- Module      : Network.AWS.ECS.PutAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or update an attribute on an Amazon ECS resource. If the attribute does not exist, it is created. If the attribute exists, its value is replaced with the specified value. To delete an attribute, use 'DeleteAttributes' . For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
module Network.AWS.ECS.PutAttributes
    (
    -- * Creating a Request
      putAttributes
    , PutAttributes
    -- * Request Lenses
    , paCluster
    , paAttributes

    -- * Destructuring the Response
    , putAttributesResponse
    , PutAttributesResponse
    -- * Response Lenses
    , parsAttributes
    , parsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putAttributes' smart constructor.
data PutAttributes = PutAttributes'
  { _paCluster    :: !(Maybe Text)
  , _paAttributes :: ![Attribute]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to apply attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'paAttributes' - The attributes to apply to your resource. You can specify up to 10 custom attributes per resource. You can specify up to 10 attributes in a single call.
putAttributes
    :: PutAttributes
putAttributes = PutAttributes' {_paCluster = Nothing, _paAttributes = mempty}


-- | The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to apply attributes. If you do not specify a cluster, the default cluster is assumed.
paCluster :: Lens' PutAttributes (Maybe Text)
paCluster = lens _paCluster (\ s a -> s{_paCluster = a})

-- | The attributes to apply to your resource. You can specify up to 10 custom attributes per resource. You can specify up to 10 attributes in a single call.
paAttributes :: Lens' PutAttributes [Attribute]
paAttributes = lens _paAttributes (\ s a -> s{_paAttributes = a}) . _Coerce

instance AWSRequest PutAttributes where
        type Rs PutAttributes = PutAttributesResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 PutAttributesResponse' <$>
                   (x .?> "attributes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable PutAttributes where

instance NFData PutAttributes where

instance ToHeaders PutAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.PutAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutAttributes where
        toJSON PutAttributes'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _paCluster,
                  Just ("attributes" .= _paAttributes)])

instance ToPath PutAttributes where
        toPath = const "/"

instance ToQuery PutAttributes where
        toQuery = const mempty

-- | /See:/ 'putAttributesResponse' smart constructor.
data PutAttributesResponse = PutAttributesResponse'
  { _parsAttributes     :: !(Maybe [Attribute])
  , _parsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parsAttributes' - The attributes applied to your resource.
--
-- * 'parsResponseStatus' - -- | The response status code.
putAttributesResponse
    :: Int -- ^ 'parsResponseStatus'
    -> PutAttributesResponse
putAttributesResponse pResponseStatus_ =
  PutAttributesResponse'
    {_parsAttributes = Nothing, _parsResponseStatus = pResponseStatus_}


-- | The attributes applied to your resource.
parsAttributes :: Lens' PutAttributesResponse [Attribute]
parsAttributes = lens _parsAttributes (\ s a -> s{_parsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
parsResponseStatus :: Lens' PutAttributesResponse Int
parsResponseStatus = lens _parsResponseStatus (\ s a -> s{_parsResponseStatus = a})

instance NFData PutAttributesResponse where
