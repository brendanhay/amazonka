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
-- Module      : Network.AWS.Lambda.UntagResource
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a Lambda function. Requires the function ARN (Amazon Resource Name).
--
--
module Network.AWS.Lambda.UntagResource
    (
    -- * Creating a Request
      untagResource
    , UntagResource
    -- * Request Lenses
    , urResource
    , urTagKeys

    -- * Destructuring the Response
    , untagResourceResponse
    , UntagResourceResponse
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
    { _urResource :: !Text
    , _urTagKeys  :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResource' - The ARN (Amazon Resource Name) of the function.
--
-- * 'urTagKeys' - The list of tag keys to be deleted from the function.
untagResource
    :: Text -- ^ 'urResource'
    -> UntagResource
untagResource pResource_ =
    UntagResource'
    { _urResource = pResource_
    , _urTagKeys = mempty
    }

-- | The ARN (Amazon Resource Name) of the function.
urResource :: Lens' UntagResource Text
urResource = lens _urResource (\ s a -> s{_urResource = a});

-- | The list of tag keys to be deleted from the function.
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a}) . _Coerce;

instance AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        request = delete lambda
        response = receiveNull UntagResourceResponse'

instance Hashable UntagResource

instance NFData UntagResource

instance ToHeaders UntagResource where
        toHeaders = const mempty

instance ToPath UntagResource where
        toPath UntagResource'{..}
          = mconcat ["/2017-03-31/tags/", toBS _urResource]

instance ToQuery UntagResource where
        toQuery UntagResource'{..}
          = mconcat
              ["tagKeys" =: toQueryList "member" _urTagKeys]

-- | /See:/ 'untagResourceResponse' smart constructor.
data UntagResourceResponse =
    UntagResourceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
untagResourceResponse
    :: UntagResourceResponse
untagResourceResponse = UntagResourceResponse'

instance NFData UntagResourceResponse
