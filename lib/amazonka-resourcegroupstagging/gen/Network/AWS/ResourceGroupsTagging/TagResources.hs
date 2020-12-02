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
-- Module      : Network.AWS.ResourceGroupsTagging.TagResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies one or more tags to the specified resources. Note the following:
--
--
--     * Not all resources can have tags. For a list of resources that support tagging, see <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/supported-resources.html Supported Resources> in the /AWS Resource Groups and Tag Editor User Guide/ .
--
--     * Each resource can have up to 50 tags. For other limits, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-restrictions Tag Restrictions> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--     * You can only tag resources that are located in the specified region for the AWS account.
--
--     * To add tags to a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for adding tags. For more information, see <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html Obtaining Permissions for Tagging> in the /AWS Resource Groups and Tag Editor User Guide/ .
--
--
--
module Network.AWS.ResourceGroupsTagging.TagResources
    (
    -- * Creating a Request
      tagResources
    , TagResources
    -- * Request Lenses
    , trResourceARNList
    , trTags

    -- * Destructuring the Response
    , tagResourcesResponse
    , TagResourcesResponse
    -- * Response Lenses
    , trrsFailedResourcesMap
    , trrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.ResourceGroupsTagging.Types.Product
import Network.AWS.Response

-- | /See:/ 'tagResources' smart constructor.
data TagResources = TagResources'
  { _trResourceARNList :: !(List1 Text)
  , _trTags            :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARNList' - A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. You can specify a minimum of 1 and a maximum of 20 ARNs (resources) to tag. An ARN can be set to a maximum of 1600 characters. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'trTags' - The tags that you want to add to the specified resources. A tag consists of a key and a value that you define.
tagResources
    :: NonEmpty Text -- ^ 'trResourceARNList'
    -> TagResources
tagResources pResourceARNList_ =
  TagResources'
    {_trResourceARNList = _List1 # pResourceARNList_, _trTags = mempty}


-- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. You can specify a minimum of 1 and a maximum of 20 ARNs (resources) to tag. An ARN can be set to a maximum of 1600 characters. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
trResourceARNList :: Lens' TagResources (NonEmpty Text)
trResourceARNList = lens _trResourceARNList (\ s a -> s{_trResourceARNList = a}) . _List1

-- | The tags that you want to add to the specified resources. A tag consists of a key and a value that you define.
trTags :: Lens' TagResources (HashMap Text Text)
trTags = lens _trTags (\ s a -> s{_trTags = a}) . _Map

instance AWSRequest TagResources where
        type Rs TagResources = TagResourcesResponse
        request = postJSON resourceGroupsTagging
        response
          = receiveJSON
              (\ s h x ->
                 TagResourcesResponse' <$>
                   (x .?> "FailedResourcesMap" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable TagResources where

instance NFData TagResources where

instance ToHeaders TagResources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ResourceGroupsTaggingAPI_20170126.TagResources" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagResources where
        toJSON TagResources'{..}
          = object
              (catMaybes
                 [Just ("ResourceARNList" .= _trResourceARNList),
                  Just ("Tags" .= _trTags)])

instance ToPath TagResources where
        toPath = const "/"

instance ToQuery TagResources where
        toQuery = const mempty

-- | /See:/ 'tagResourcesResponse' smart constructor.
data TagResourcesResponse = TagResourcesResponse'
  { _trrsFailedResourcesMap :: !(Maybe (Map Text FailureInfo))
  , _trrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trrsFailedResourcesMap' - Details of resources that could not be tagged. An error code, status code, and error message are returned for each failed item.
--
-- * 'trrsResponseStatus' - -- | The response status code.
tagResourcesResponse
    :: Int -- ^ 'trrsResponseStatus'
    -> TagResourcesResponse
tagResourcesResponse pResponseStatus_ =
  TagResourcesResponse'
    {_trrsFailedResourcesMap = Nothing, _trrsResponseStatus = pResponseStatus_}


-- | Details of resources that could not be tagged. An error code, status code, and error message are returned for each failed item.
trrsFailedResourcesMap :: Lens' TagResourcesResponse (HashMap Text FailureInfo)
trrsFailedResourcesMap = lens _trrsFailedResourcesMap (\ s a -> s{_trrsFailedResourcesMap = a}) . _Default . _Map

-- | -- | The response status code.
trrsResponseStatus :: Lens' TagResourcesResponse Int
trrsResponseStatus = lens _trrsResponseStatus (\ s a -> s{_trrsResponseStatus = a})

instance NFData TagResourcesResponse where
