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
-- Module      : Network.AWS.CloudHSM.AddTagsToResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
--
-- Adds or overwrites one or more tags for the specified AWS CloudHSM resource.
--
-- Each tag consists of a key and a value. Tag keys must be unique to each resource.
--
module Network.AWS.CloudHSM.AddTagsToResource
    (
    -- * Creating a Request
      addTagsToResource
    , AddTagsToResource
    -- * Request Lenses
    , attrResourceARN
    , attrTagList

    -- * Destructuring the Response
    , addTagsToResourceResponse
    , AddTagsToResourceResponse
    -- * Response Lenses
    , attrrsResponseStatus
    , attrrsStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { _attrResourceARN :: !Text
  , _attrTagList     :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrResourceARN' - The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
--
-- * 'attrTagList' - One or more tags.
addTagsToResource
    :: Text -- ^ 'attrResourceARN'
    -> AddTagsToResource
addTagsToResource pResourceARN_ =
  AddTagsToResource' {_attrResourceARN = pResourceARN_, _attrTagList = mempty}


-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
attrResourceARN :: Lens' AddTagsToResource Text
attrResourceARN = lens _attrResourceARN (\ s a -> s{_attrResourceARN = a})

-- | One or more tags.
attrTagList :: Lens' AddTagsToResource [Tag]
attrTagList = lens _attrTagList (\ s a -> s{_attrTagList = a}) . _Coerce

instance AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 AddTagsToResourceResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Status"))

instance Hashable AddTagsToResource where

instance NFData AddTagsToResource where

instance ToHeaders AddTagsToResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.AddTagsToResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTagsToResource where
        toJSON AddTagsToResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _attrResourceARN),
                  Just ("TagList" .= _attrTagList)])

instance ToPath AddTagsToResource where
        toPath = const "/"

instance ToQuery AddTagsToResource where
        toQuery = const mempty

-- | /See:/ 'addTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { _attrrsResponseStatus :: !Int
  , _attrrsStatus         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrrsResponseStatus' - -- | The response status code.
--
-- * 'attrrsStatus' - The status of the operation.
addTagsToResourceResponse
    :: Int -- ^ 'attrrsResponseStatus'
    -> Text -- ^ 'attrrsStatus'
    -> AddTagsToResourceResponse
addTagsToResourceResponse pResponseStatus_ pStatus_ =
  AddTagsToResourceResponse'
    {_attrrsResponseStatus = pResponseStatus_, _attrrsStatus = pStatus_}


-- | -- | The response status code.
attrrsResponseStatus :: Lens' AddTagsToResourceResponse Int
attrrsResponseStatus = lens _attrrsResponseStatus (\ s a -> s{_attrrsResponseStatus = a})

-- | The status of the operation.
attrrsStatus :: Lens' AddTagsToResourceResponse Text
attrrsStatus = lens _attrrsStatus (\ s a -> s{_attrrsStatus = a})

instance NFData AddTagsToResourceResponse where
