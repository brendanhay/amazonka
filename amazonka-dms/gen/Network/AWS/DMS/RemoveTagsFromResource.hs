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
-- Module      : Network.AWS.DMS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from a DMS resource.
--
--
module Network.AWS.DMS.RemoveTagsFromResource
    (
    -- * Creating a Request
      removeTagsFromResource
    , RemoveTagsFromResource
    -- * Request Lenses
    , rtfrResourceARN
    , rtfrTagKeys

    -- * Destructuring the Response
    , removeTagsFromResourceResponse
    , RemoveTagsFromResourceResponse
    -- * Response Lenses
    , rtfrrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'removeTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { _rtfrResourceARN :: !Text
  , _rtfrTagKeys     :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrResourceARN' - >The Amazon Resource Name (ARN) of the AWS DMS resource the tag is to be removed from.
--
-- * 'rtfrTagKeys' - The tag key (name) of the tag to be removed.
removeTagsFromResource
    :: Text -- ^ 'rtfrResourceARN'
    -> RemoveTagsFromResource
removeTagsFromResource pResourceARN_ =
  RemoveTagsFromResource'
    {_rtfrResourceARN = pResourceARN_, _rtfrTagKeys = mempty}


-- | >The Amazon Resource Name (ARN) of the AWS DMS resource the tag is to be removed from.
rtfrResourceARN :: Lens' RemoveTagsFromResource Text
rtfrResourceARN = lens _rtfrResourceARN (\ s a -> s{_rtfrResourceARN = a})

-- | The tag key (name) of the tag to be removed.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\ s a -> s{_rtfrTagKeys = a}) . _Coerce

instance AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource =
             RemoveTagsFromResourceResponse
        request = postJSON dms
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveTagsFromResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RemoveTagsFromResource where

instance NFData RemoveTagsFromResource where

instance ToHeaders RemoveTagsFromResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.RemoveTagsFromResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTagsFromResource where
        toJSON RemoveTagsFromResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _rtfrResourceARN),
                  Just ("TagKeys" .= _rtfrTagKeys)])

instance ToPath RemoveTagsFromResource where
        toPath = const "/"

instance ToQuery RemoveTagsFromResource where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'removeTagsFromResourceResponse' smart constructor.
newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { _rtfrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrrsResponseStatus' - -- | The response status code.
removeTagsFromResourceResponse
    :: Int -- ^ 'rtfrrsResponseStatus'
    -> RemoveTagsFromResourceResponse
removeTagsFromResourceResponse pResponseStatus_ =
  RemoveTagsFromResourceResponse' {_rtfrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rtfrrsResponseStatus :: Lens' RemoveTagsFromResourceResponse Int
rtfrrsResponseStatus = lens _rtfrrsResponseStatus (\ s a -> s{_rtfrrsResponseStatus = a})

instance NFData RemoveTagsFromResourceResponse where
