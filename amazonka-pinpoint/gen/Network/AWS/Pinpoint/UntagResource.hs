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
-- Module      : Network.AWS.Pinpoint.UntagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.Pinpoint.UntagResource
    (
    -- * Creating a Request
      untagResource
    , UntagResource
    -- * Request Lenses
    , urTagKeys
    , urResourceARN

    -- * Destructuring the Response
    , untagResourceResponse
    , UntagResourceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urTagKeys     :: ![Text]
  , _urResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urTagKeys' - The key(s) of tag to be deleted
--
-- * 'urResourceARN' - Undocumented member.
untagResource
    :: Text -- ^ 'urResourceARN'
    -> UntagResource
untagResource pResourceARN_ =
  UntagResource' {_urTagKeys = mempty, _urResourceARN = pResourceARN_}


-- | The key(s) of tag to be deleted
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a}) . _Coerce

-- | Undocumented member.
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\ s a -> s{_urResourceARN = a})

instance AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        request = delete pinpoint
        response = receiveNull UntagResourceResponse'

instance Hashable UntagResource where

instance NFData UntagResource where

instance ToHeaders UntagResource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath UntagResource where
        toPath UntagResource'{..}
          = mconcat ["/v1/tags/", toBS _urResourceARN]

instance ToQuery UntagResource where
        toQuery UntagResource'{..}
          = mconcat
              ["tagKeys" =: toQueryList "member" _urTagKeys]

-- | /See:/ 'untagResourceResponse' smart constructor.
data UntagResourceResponse =
  UntagResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
untagResourceResponse
    :: UntagResourceResponse
untagResourceResponse = UntagResourceResponse'


instance NFData UntagResourceResponse where
