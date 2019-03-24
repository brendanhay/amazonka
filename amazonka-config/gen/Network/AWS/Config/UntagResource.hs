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
-- Module      : Network.AWS.Config.UntagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes specified tags from a resource.
--
--
module Network.AWS.Config.UntagResource
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
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
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
-- * 'urResourceARN' - The Amazon Resource Name (ARN) that identifies the resource for which to list the tags. Currently, the supported resources are @ConfigRule@ , @ConfigurationAggregator@ and @AggregatorAuthorization@ .
--
-- * 'urTagKeys' - The keys of the tags to be removed.
untagResource
    :: Text -- ^ 'urResourceARN'
    -> NonEmpty Text -- ^ 'urTagKeys'
    -> UntagResource
untagResource pResourceARN_ pTagKeys_ =
  UntagResource'
    {_urResourceARN = pResourceARN_, _urTagKeys = _List1 # pTagKeys_}


-- | The Amazon Resource Name (ARN) that identifies the resource for which to list the tags. Currently, the supported resources are @ConfigRule@ , @ConfigurationAggregator@ and @AggregatorAuthorization@ .
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\ s a -> s{_urResourceARN = a})

-- | The keys of the tags to be removed.
urTagKeys :: Lens' UntagResource (NonEmpty Text)
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a}) . _List1

instance AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        request = postJSON config
        response = receiveNull UntagResourceResponse'

instance Hashable UntagResource where

instance NFData UntagResource where

instance ToHeaders UntagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.UntagResource" :: ByteString),
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
data UntagResourceResponse =
  UntagResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
untagResourceResponse
    :: UntagResourceResponse
untagResourceResponse = UntagResourceResponse'


instance NFData UntagResourceResponse where
