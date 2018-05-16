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
-- Module      : Network.AWS.SSM.AddTagsToResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified resource. Tags are metadata that you can assign to your documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value, both of which you define. For example, you could define a set of tags for your account's managed instances that helps you track each instance's owner and stack level. For example: Key=Owner and Value=DbAdmin, SysAdmin, or Dev. Or Key=Stack and Value=Production, Pre-Production, or Test.
--
--
-- Each resource can have a maximum of 50 tags.
--
-- We recommend that you devise a set of tag keys that meets your needs for each resource type. Using a consistent set of tag keys makes it easier for you to manage your resources. You can search and filter the resources based on the tags you add. Tags don't have any semantic meaning to Amazon EC2 and are interpreted strictly as a string of characters.
--
-- For more information about tags, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Amazon EC2 Resources> in the /Amazon EC2 User Guide/ .
--
module Network.AWS.SSM.AddTagsToResource
    (
    -- * Creating a Request
      addTagsToResource
    , AddTagsToResource
    -- * Request Lenses
    , attrResourceType
    , attrResourceId
    , attrTags

    -- * Destructuring the Response
    , addTagsToResourceResponse
    , AddTagsToResourceResponse
    -- * Response Lenses
    , attrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'addTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { _attrResourceType :: !ResourceTypeForTagging
  , _attrResourceId   :: !Text
  , _attrTags         :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrResourceType' - Specifies the type of resource you are tagging.
--
-- * 'attrResourceId' - The resource ID you want to tag. Use the ID of the resource. Here are some examples: ManagedInstance: mi-012345abcde MaintenanceWindow: mw-012345abcde PatchBaseline: pb-012345abcde For the Document and Parameter values, use the name of the resource.
--
-- * 'attrTags' - One or more tags. The value parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.  /Important:/ Do not enter personally identifiable information in this field.
addTagsToResource
    :: ResourceTypeForTagging -- ^ 'attrResourceType'
    -> Text -- ^ 'attrResourceId'
    -> AddTagsToResource
addTagsToResource pResourceType_ pResourceId_ =
  AddTagsToResource'
    { _attrResourceType = pResourceType_
    , _attrResourceId = pResourceId_
    , _attrTags = mempty
    }


-- | Specifies the type of resource you are tagging.
attrResourceType :: Lens' AddTagsToResource ResourceTypeForTagging
attrResourceType = lens _attrResourceType (\ s a -> s{_attrResourceType = a})

-- | The resource ID you want to tag. Use the ID of the resource. Here are some examples: ManagedInstance: mi-012345abcde MaintenanceWindow: mw-012345abcde PatchBaseline: pb-012345abcde For the Document and Parameter values, use the name of the resource.
attrResourceId :: Lens' AddTagsToResource Text
attrResourceId = lens _attrResourceId (\ s a -> s{_attrResourceId = a})

-- | One or more tags. The value parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.  /Important:/ Do not enter personally identifiable information in this field.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\ s a -> s{_attrTags = a}) . _Coerce

instance AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 AddTagsToResourceResponse' <$> (pure (fromEnum s)))

instance Hashable AddTagsToResource where

instance NFData AddTagsToResource where

instance ToHeaders AddTagsToResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.AddTagsToResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTagsToResource where
        toJSON AddTagsToResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceType" .= _attrResourceType),
                  Just ("ResourceId" .= _attrResourceId),
                  Just ("Tags" .= _attrTags)])

instance ToPath AddTagsToResource where
        toPath = const "/"

instance ToQuery AddTagsToResource where
        toQuery = const mempty

-- | /See:/ 'addTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { _attrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrrsResponseStatus' - -- | The response status code.
addTagsToResourceResponse
    :: Int -- ^ 'attrrsResponseStatus'
    -> AddTagsToResourceResponse
addTagsToResourceResponse pResponseStatus_ =
  AddTagsToResourceResponse' {_attrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
attrrsResponseStatus :: Lens' AddTagsToResourceResponse Int
attrrsResponseStatus = lens _attrrsResponseStatus (\ s a -> s{_attrrsResponseStatus = a})

instance NFData AddTagsToResourceResponse where
