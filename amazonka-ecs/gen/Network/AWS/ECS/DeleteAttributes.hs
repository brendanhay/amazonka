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
-- Module      : Network.AWS.ECS.DeleteAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more custom attributes from an Amazon ECS resource.
--
--
module Network.AWS.ECS.DeleteAttributes
    (
    -- * Creating a Request
      deleteAttributes
    , DeleteAttributes
    -- * Request Lenses
    , daCluster
    , daAttributes

    -- * Destructuring the Response
    , deleteAttributesResponse
    , DeleteAttributesResponse
    -- * Response Lenses
    , darsAttributes
    , darsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { _daCluster    :: !(Maybe Text)
  , _daAttributes :: ![Attribute]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to delete attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'daAttributes' - The attributes to delete from your resource. You can specify up to 10 attributes per request. For custom attributes, specify the attribute name and target ID, but do not specify the value. If you specify the target ID using the short form, you must also specify the target type.
deleteAttributes
    :: DeleteAttributes
deleteAttributes =
  DeleteAttributes' {_daCluster = Nothing, _daAttributes = mempty}


-- | The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to delete attributes. If you do not specify a cluster, the default cluster is assumed.
daCluster :: Lens' DeleteAttributes (Maybe Text)
daCluster = lens _daCluster (\ s a -> s{_daCluster = a})

-- | The attributes to delete from your resource. You can specify up to 10 attributes per request. For custom attributes, specify the attribute name and target ID, but do not specify the value. If you specify the target ID using the short form, you must also specify the target type.
daAttributes :: Lens' DeleteAttributes [Attribute]
daAttributes = lens _daAttributes (\ s a -> s{_daAttributes = a}) . _Coerce

instance AWSRequest DeleteAttributes where
        type Rs DeleteAttributes = DeleteAttributesResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAttributesResponse' <$>
                   (x .?> "attributes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteAttributes where

instance NFData DeleteAttributes where

instance ToHeaders DeleteAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DeleteAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAttributes where
        toJSON DeleteAttributes'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _daCluster,
                  Just ("attributes" .= _daAttributes)])

instance ToPath DeleteAttributes where
        toPath = const "/"

instance ToQuery DeleteAttributes where
        toQuery = const mempty

-- | /See:/ 'deleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  { _darsAttributes     :: !(Maybe [Attribute])
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAttributes' - A list of attribute objects that were successfully deleted from your resource.
--
-- * 'darsResponseStatus' - -- | The response status code.
deleteAttributesResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DeleteAttributesResponse
deleteAttributesResponse pResponseStatus_ =
  DeleteAttributesResponse'
    {_darsAttributes = Nothing, _darsResponseStatus = pResponseStatus_}


-- | A list of attribute objects that were successfully deleted from your resource.
darsAttributes :: Lens' DeleteAttributesResponse [Attribute]
darsAttributes = lens _darsAttributes (\ s a -> s{_darsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteAttributesResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DeleteAttributesResponse where
