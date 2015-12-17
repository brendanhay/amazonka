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
-- Module      : Network.AWS.Inspector.CreateResourceGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group using the specified set of tags (key and value
-- pairs) that are used to select the EC2 instances to be included in an
-- Inspector application. The created resource group is then used to create
-- an Inspector application.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_CreateResourceGroup.html AWS API Reference> for CreateResourceGroup.
module Network.AWS.Inspector.CreateResourceGroup
    (
    -- * Creating a Request
      createResourceGroup
    , CreateResourceGroup
    -- * Request Lenses
    , crgResourceGroupTags

    -- * Destructuring the Response
    , createResourceGroupResponse
    , CreateResourceGroupResponse
    -- * Response Lenses
    , crgrsResourceGroupARN
    , crgrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createResourceGroup' smart constructor.
newtype CreateResourceGroup = CreateResourceGroup'
    { _crgResourceGroupTags :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateResourceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgResourceGroupTags'
createResourceGroup
    :: CreateResourceGroup
createResourceGroup =
    CreateResourceGroup'
    { _crgResourceGroupTags = Nothing
    }

-- | A collection of keys and an array of possible values in JSON format.
--
-- For example, [{ \"key1\" : [\"Value1\",\"Value2\"]},{\"Key2\":
-- [\"Value3\"]}]
crgResourceGroupTags :: Lens' CreateResourceGroup (Maybe Text)
crgResourceGroupTags = lens _crgResourceGroupTags (\ s a -> s{_crgResourceGroupTags = a});

instance AWSRequest CreateResourceGroup where
        type Rs CreateResourceGroup =
             CreateResourceGroupResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 CreateResourceGroupResponse' <$>
                   (x .?> "resourceGroupArn") <*> (pure (fromEnum s)))

instance ToHeaders CreateResourceGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.CreateResourceGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateResourceGroup where
        toJSON CreateResourceGroup'{..}
          = object
              (catMaybes
                 [("resourceGroupTags" .=) <$> _crgResourceGroupTags])

instance ToPath CreateResourceGroup where
        toPath = const "/"

instance ToQuery CreateResourceGroup where
        toQuery = const mempty

-- | /See:/ 'createResourceGroupResponse' smart constructor.
data CreateResourceGroupResponse = CreateResourceGroupResponse'
    { _crgrsResourceGroupARN :: !(Maybe Text)
    , _crgrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateResourceGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgrsResourceGroupARN'
--
-- * 'crgrsResponseStatus'
createResourceGroupResponse
    :: Int -- ^ 'crgrsResponseStatus'
    -> CreateResourceGroupResponse
createResourceGroupResponse pResponseStatus_ =
    CreateResourceGroupResponse'
    { _crgrsResourceGroupARN = Nothing
    , _crgrsResponseStatus = pResponseStatus_
    }

-- | The ARN specifying the resource group that is created.
crgrsResourceGroupARN :: Lens' CreateResourceGroupResponse (Maybe Text)
crgrsResourceGroupARN = lens _crgrsResourceGroupARN (\ s a -> s{_crgrsResourceGroupARN = a});

-- | The response status code.
crgrsResponseStatus :: Lens' CreateResourceGroupResponse Int
crgrsResponseStatus = lens _crgrsResponseStatus (\ s a -> s{_crgrsResponseStatus = a});
