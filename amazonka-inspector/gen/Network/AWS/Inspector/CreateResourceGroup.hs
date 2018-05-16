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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group using the specified set of tags (key and value pairs) that are used to select the EC2 instances to be included in an Amazon Inspector assessment target. The created resource group is then used to create an Amazon Inspector assessment target. For more information, see 'CreateAssessmentTarget' .
--
--
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
    , crgrsResponseStatus
    , crgrsResourceGroupARN
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createResourceGroup' smart constructor.
newtype CreateResourceGroup = CreateResourceGroup'
  { _crgResourceGroupTags :: List1 ResourceGroupTag
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgResourceGroupTags' - A collection of keys and an array of possible values, '[{"key":"key1","values":["Value1","Value2"]},{"key":"Key2","values":["Value3"]}]'. For example,'[{"key":"Name","values":["TestEC2Instance"]}]'.
createResourceGroup
    :: NonEmpty ResourceGroupTag -- ^ 'crgResourceGroupTags'
    -> CreateResourceGroup
createResourceGroup pResourceGroupTags_ =
  CreateResourceGroup' {_crgResourceGroupTags = _List1 # pResourceGroupTags_}


-- | A collection of keys and an array of possible values, '[{"key":"key1","values":["Value1","Value2"]},{"key":"Key2","values":["Value3"]}]'. For example,'[{"key":"Name","values":["TestEC2Instance"]}]'.
crgResourceGroupTags :: Lens' CreateResourceGroup (NonEmpty ResourceGroupTag)
crgResourceGroupTags = lens _crgResourceGroupTags (\ s a -> s{_crgResourceGroupTags = a}) . _List1

instance AWSRequest CreateResourceGroup where
        type Rs CreateResourceGroup =
             CreateResourceGroupResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 CreateResourceGroupResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "resourceGroupArn"))

instance Hashable CreateResourceGroup where

instance NFData CreateResourceGroup where

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
                 [Just
                    ("resourceGroupTags" .= _crgResourceGroupTags)])

instance ToPath CreateResourceGroup where
        toPath = const "/"

instance ToQuery CreateResourceGroup where
        toQuery = const mempty

-- | /See:/ 'createResourceGroupResponse' smart constructor.
data CreateResourceGroupResponse = CreateResourceGroupResponse'
  { _crgrsResponseStatus   :: !Int
  , _crgrsResourceGroupARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgrsResponseStatus' - -- | The response status code.
--
-- * 'crgrsResourceGroupARN' - The ARN that specifies the resource group that is created.
createResourceGroupResponse
    :: Int -- ^ 'crgrsResponseStatus'
    -> Text -- ^ 'crgrsResourceGroupARN'
    -> CreateResourceGroupResponse
createResourceGroupResponse pResponseStatus_ pResourceGroupARN_ =
  CreateResourceGroupResponse'
    { _crgrsResponseStatus = pResponseStatus_
    , _crgrsResourceGroupARN = pResourceGroupARN_
    }


-- | -- | The response status code.
crgrsResponseStatus :: Lens' CreateResourceGroupResponse Int
crgrsResponseStatus = lens _crgrsResponseStatus (\ s a -> s{_crgrsResponseStatus = a})

-- | The ARN that specifies the resource group that is created.
crgrsResourceGroupARN :: Lens' CreateResourceGroupResponse Text
crgrsResourceGroupARN = lens _crgrsResourceGroupARN (\ s a -> s{_crgrsResourceGroupARN = a})

instance NFData CreateResourceGroupResponse where
