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
-- Module      : Network.AWS.DirectConnect.TagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified Direct Connect resource. Each Direct Connect resource can have a maximum of 50 tags.
--
--
-- Each tag consists of a key and an optional value. If a tag with the same key is already associated with the Direct Connect resource, this action updates its value.
--
module Network.AWS.DirectConnect.TagResource
    (
    -- * Creating a Request
      tagResource
    , TagResource
    -- * Request Lenses
    , trResourceARN
    , trTags

    -- * Destructuring the Response
    , tagResourceResponse
    , TagResourceResponse
    -- * Response Lenses
    , trrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the TagResource operation.
--
--
--
-- /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceARN :: !Text
  , _trTags        :: !(List1 Tag)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - The Amazon Resource Name (ARN) of the Direct Connect resource. Example: arn:aws:directconnect:us-east-1:123456789012:dxcon/dxcon-fg5678gh
--
-- * 'trTags' - The list of tags to add.
tagResource
    :: Text -- ^ 'trResourceARN'
    -> NonEmpty Tag -- ^ 'trTags'
    -> TagResource
tagResource pResourceARN_ pTags_ =
  TagResource' {_trResourceARN = pResourceARN_, _trTags = _List1 # pTags_}


-- | The Amazon Resource Name (ARN) of the Direct Connect resource. Example: arn:aws:directconnect:us-east-1:123456789012:dxcon/dxcon-fg5678gh
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\ s a -> s{_trResourceARN = a})

-- | The list of tags to add.
trTags :: Lens' TagResource (NonEmpty Tag)
trTags = lens _trTags (\ s a -> s{_trTags = a}) . _List1

instance AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        request = postJSON directConnect
        response
          = receiveEmpty
              (\ s h x ->
                 TagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable TagResource where

instance NFData TagResource where

instance ToHeaders TagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.TagResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagResource where
        toJSON TagResource'{..}
          = object
              (catMaybes
                 [Just ("resourceArn" .= _trResourceARN),
                  Just ("tags" .= _trTags)])

instance ToPath TagResource where
        toPath = const "/"

instance ToQuery TagResource where
        toQuery = const mempty

-- | The response received when TagResource is called.
--
--
--
-- /See:/ 'tagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
  { _trrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trrsResponseStatus' - -- | The response status code.
tagResourceResponse
    :: Int -- ^ 'trrsResponseStatus'
    -> TagResourceResponse
tagResourceResponse pResponseStatus_ =
  TagResourceResponse' {_trrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
trrsResponseStatus :: Lens' TagResourceResponse Int
trrsResponseStatus = lens _trrsResponseStatus (\ s a -> s{_trrsResponseStatus = a})

instance NFData TagResourceResponse where
