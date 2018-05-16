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
-- Module      : Network.AWS.ResourceGroupsTagging.UntagResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified resources. When you specify a tag key, the action removes both that key and its associated value. The operation succeeds even if you attempt to remove tags from a resource that were already removed. Note the following:
--
--
--     * To remove tags from a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for removing tags. For more information, see <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html Obtaining Permissions for Tagging> in the /AWS Resource Groups and Tag Editor User Guide/ .
--
--     * You can only tag resources that are located in the specified region for the AWS account.
--
--
--
module Network.AWS.ResourceGroupsTagging.UntagResources
    (
    -- * Creating a Request
      untagResources
    , UntagResources
    -- * Request Lenses
    , urResourceARNList
    , urTagKeys

    -- * Destructuring the Response
    , untagResourcesResponse
    , UntagResourcesResponse
    -- * Response Lenses
    , urrsFailedResourcesMap
    , urrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.ResourceGroupsTagging.Types.Product
import Network.AWS.Response

-- | /See:/ 'untagResources' smart constructor.
data UntagResources = UntagResources'
  { _urResourceARNList :: !(List1 Text)
  , _urTagKeys         :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceARNList' - A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. You can specify a minimum of 1 and a maximum of 20 ARNs (resources) to untag. An ARN can be set to a maximum of 1600 characters. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'urTagKeys' - A list of the tag keys that you want to remove from the specified resources.
untagResources
    :: NonEmpty Text -- ^ 'urResourceARNList'
    -> NonEmpty Text -- ^ 'urTagKeys'
    -> UntagResources
untagResources pResourceARNList_ pTagKeys_ =
  UntagResources'
    { _urResourceARNList = _List1 # pResourceARNList_
    , _urTagKeys = _List1 # pTagKeys_
    }


-- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. You can specify a minimum of 1 and a maximum of 20 ARNs (resources) to untag. An ARN can be set to a maximum of 1600 characters. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
urResourceARNList :: Lens' UntagResources (NonEmpty Text)
urResourceARNList = lens _urResourceARNList (\ s a -> s{_urResourceARNList = a}) . _List1

-- | A list of the tag keys that you want to remove from the specified resources.
urTagKeys :: Lens' UntagResources (NonEmpty Text)
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a}) . _List1

instance AWSRequest UntagResources where
        type Rs UntagResources = UntagResourcesResponse
        request = postJSON resourceGroupsTagging
        response
          = receiveJSON
              (\ s h x ->
                 UntagResourcesResponse' <$>
                   (x .?> "FailedResourcesMap" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable UntagResources where

instance NFData UntagResources where

instance ToHeaders UntagResources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ResourceGroupsTaggingAPI_20170126.UntagResources"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UntagResources where
        toJSON UntagResources'{..}
          = object
              (catMaybes
                 [Just ("ResourceARNList" .= _urResourceARNList),
                  Just ("TagKeys" .= _urTagKeys)])

instance ToPath UntagResources where
        toPath = const "/"

instance ToQuery UntagResources where
        toQuery = const mempty

-- | /See:/ 'untagResourcesResponse' smart constructor.
data UntagResourcesResponse = UntagResourcesResponse'
  { _urrsFailedResourcesMap :: !(Maybe (Map Text FailureInfo))
  , _urrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsFailedResourcesMap' - Details of resources that could not be untagged. An error code, status code, and error message are returned for each failed item.
--
-- * 'urrsResponseStatus' - -- | The response status code.
untagResourcesResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UntagResourcesResponse
untagResourcesResponse pResponseStatus_ =
  UntagResourcesResponse'
    {_urrsFailedResourcesMap = Nothing, _urrsResponseStatus = pResponseStatus_}


-- | Details of resources that could not be untagged. An error code, status code, and error message are returned for each failed item.
urrsFailedResourcesMap :: Lens' UntagResourcesResponse (HashMap Text FailureInfo)
urrsFailedResourcesMap = lens _urrsFailedResourcesMap (\ s a -> s{_urrsFailedResourcesMap = a}) . _Default . _Map

-- | -- | The response status code.
urrsResponseStatus :: Lens' UntagResourcesResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UntagResourcesResponse where
