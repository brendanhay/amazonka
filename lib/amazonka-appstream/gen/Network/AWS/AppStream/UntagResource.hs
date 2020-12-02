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
-- Module      : Network.AWS.AppStream.UntagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified tags from the specified AppStream 2.0 resource.
--
--
-- To list the current tags for your resources, use 'ListTagsForResource' .
--
-- For more information about tags, see <http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Developer Guide/ .
--
module Network.AWS.AppStream.UntagResource
    (
    -- * Creating a Request
      untagResource
    , UntagResource
    -- * Request Lenses
    , urResourceARN
    , urTagKeys

    -- * Destructuring the Response
    , untagResourceResponse
    , UntagResourceResponse
    -- * Response Lenses
    , urrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urResourceARN :: !Text
  , _urTagKeys     :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'urTagKeys' - The tag keys for the tags to disassociate.
untagResource
    :: Text -- ^ 'urResourceARN'
    -> NonEmpty Text -- ^ 'urTagKeys'
    -> UntagResource
untagResource pResourceARN_ pTagKeys_ =
  UntagResource'
    {_urResourceARN = pResourceARN_, _urTagKeys = _List1 # pTagKeys_}


-- | The Amazon Resource Name (ARN) of the resource.
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\ s a -> s{_urResourceARN = a})

-- | The tag keys for the tags to disassociate.
urTagKeys :: Lens' UntagResource (NonEmpty Text)
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a}) . _List1

instance AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 UntagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable UntagResource where

instance NFData UntagResource where

instance ToHeaders UntagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.UntagResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UntagResource where
        toJSON UntagResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _urResourceARN),
                  Just ("TagKeys" .= _urTagKeys)])

instance ToPath UntagResource where
        toPath = const "/"

instance ToQuery UntagResource where
        toQuery = const mempty

-- | /See:/ 'untagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { _urrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsResponseStatus' - -- | The response status code.
untagResourceResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UntagResourceResponse
untagResourceResponse pResponseStatus_ =
  UntagResourceResponse' {_urrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
urrsResponseStatus :: Lens' UntagResourceResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UntagResourceResponse where
