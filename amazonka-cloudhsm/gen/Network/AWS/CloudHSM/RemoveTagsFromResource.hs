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
-- Module      : Network.AWS.CloudHSM.RemoveTagsFromResource
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
-- Removes one or more tags from the specified AWS CloudHSM resource.
--
-- To remove a tag, specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
--
module Network.AWS.CloudHSM.RemoveTagsFromResource
    (
    -- * Creating a Request
      removeTagsFromResource
    , RemoveTagsFromResource
    -- * Request Lenses
    , rtfrResourceARN
    , rtfrTagKeyList

    -- * Destructuring the Response
    , removeTagsFromResourceResponse
    , RemoveTagsFromResourceResponse
    -- * Response Lenses
    , rtfrrsResponseStatus
    , rtfrrsStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { _rtfrResourceARN :: !Text
  , _rtfrTagKeyList  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrResourceARN' - The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
--
-- * 'rtfrTagKeyList' - The tag key or keys to remove. Specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
removeTagsFromResource
    :: Text -- ^ 'rtfrResourceARN'
    -> RemoveTagsFromResource
removeTagsFromResource pResourceARN_ =
  RemoveTagsFromResource'
    {_rtfrResourceARN = pResourceARN_, _rtfrTagKeyList = mempty}


-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
rtfrResourceARN :: Lens' RemoveTagsFromResource Text
rtfrResourceARN = lens _rtfrResourceARN (\ s a -> s{_rtfrResourceARN = a})

-- | The tag key or keys to remove. Specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
rtfrTagKeyList :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeyList = lens _rtfrTagKeyList (\ s a -> s{_rtfrTagKeyList = a}) . _Coerce

instance AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource =
             RemoveTagsFromResourceResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 RemoveTagsFromResourceResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Status"))

instance Hashable RemoveTagsFromResource where

instance NFData RemoveTagsFromResource where

instance ToHeaders RemoveTagsFromResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.RemoveTagsFromResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTagsFromResource where
        toJSON RemoveTagsFromResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _rtfrResourceARN),
                  Just ("TagKeyList" .= _rtfrTagKeyList)])

instance ToPath RemoveTagsFromResource where
        toPath = const "/"

instance ToQuery RemoveTagsFromResource where
        toQuery = const mempty

-- | /See:/ 'removeTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { _rtfrrsResponseStatus :: !Int
  , _rtfrrsStatus         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrrsResponseStatus' - -- | The response status code.
--
-- * 'rtfrrsStatus' - The status of the operation.
removeTagsFromResourceResponse
    :: Int -- ^ 'rtfrrsResponseStatus'
    -> Text -- ^ 'rtfrrsStatus'
    -> RemoveTagsFromResourceResponse
removeTagsFromResourceResponse pResponseStatus_ pStatus_ =
  RemoveTagsFromResourceResponse'
    {_rtfrrsResponseStatus = pResponseStatus_, _rtfrrsStatus = pStatus_}


-- | -- | The response status code.
rtfrrsResponseStatus :: Lens' RemoveTagsFromResourceResponse Int
rtfrrsResponseStatus = lens _rtfrrsResponseStatus (\ s a -> s{_rtfrrsResponseStatus = a})

-- | The status of the operation.
rtfrrsStatus :: Lens' RemoveTagsFromResourceResponse Text
rtfrrsStatus = lens _rtfrrsStatus (\ s a -> s{_rtfrrsStatus = a})

instance NFData RemoveTagsFromResourceResponse where
