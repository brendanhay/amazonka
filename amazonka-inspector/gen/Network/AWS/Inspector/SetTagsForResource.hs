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
-- Module      : Network.AWS.Inspector.SetTagsForResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets tags (key and value pairs) to the assessment specified by the
-- assessment ARN.
module Network.AWS.Inspector.SetTagsForResource
    (
    -- * Creating a Request
      setTagsForResource
    , SetTagsForResource
    -- * Request Lenses
    , stfrTags
    , stfrResourceARN

    -- * Destructuring the Response
    , setTagsForResourceResponse
    , SetTagsForResourceResponse
    -- * Response Lenses
    , stfrrsMessage
    , stfrrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setTagsForResource' smart constructor.
data SetTagsForResource = SetTagsForResource'
    { _stfrTags        :: !(Maybe [Tag])
    , _stfrResourceARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stfrTags'
--
-- * 'stfrResourceARN'
setTagsForResource
    :: Text -- ^ 'stfrResourceARN'
    -> SetTagsForResource
setTagsForResource pResourceARN_ =
    SetTagsForResource'
    { _stfrTags = Nothing
    , _stfrResourceARN = pResourceARN_
    }

-- | A collection of key and value pairs that you want to set to an
-- assessment.
stfrTags :: Lens' SetTagsForResource [Tag]
stfrTags = lens _stfrTags (\ s a -> s{_stfrTags = a}) . _Default . _Coerce;

-- | The ARN of the assessment that you want to set tags to.
stfrResourceARN :: Lens' SetTagsForResource Text
stfrResourceARN = lens _stfrResourceARN (\ s a -> s{_stfrResourceARN = a});

instance AWSRequest SetTagsForResource where
        type Rs SetTagsForResource =
             SetTagsForResourceResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 SetTagsForResourceResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance ToHeaders SetTagsForResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.SetTagsForResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetTagsForResource where
        toJSON SetTagsForResource'{..}
          = object
              (catMaybes
                 [("tags" .=) <$> _stfrTags,
                  Just ("resourceArn" .= _stfrResourceARN)])

instance ToPath SetTagsForResource where
        toPath = const "/"

instance ToQuery SetTagsForResource where
        toQuery = const mempty

-- | /See:/ 'setTagsForResourceResponse' smart constructor.
data SetTagsForResourceResponse = SetTagsForResourceResponse'
    { _stfrrsMessage        :: !(Maybe Text)
    , _stfrrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stfrrsMessage'
--
-- * 'stfrrsResponseStatus'
setTagsForResourceResponse
    :: Int -- ^ 'stfrrsResponseStatus'
    -> SetTagsForResourceResponse
setTagsForResourceResponse pResponseStatus_ =
    SetTagsForResourceResponse'
    { _stfrrsMessage = Nothing
    , _stfrrsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
stfrrsMessage :: Lens' SetTagsForResourceResponse (Maybe Text)
stfrrsMessage = lens _stfrrsMessage (\ s a -> s{_stfrrsMessage = a});

-- | The response status code.
stfrrsResponseStatus :: Lens' SetTagsForResourceResponse Int
stfrrsResponseStatus = lens _stfrrsResponseStatus (\ s a -> s{_stfrrsResponseStatus = a});
