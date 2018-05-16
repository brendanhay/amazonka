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
-- Module      : Network.AWS.CloudFront.UntagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove tags from a CloudFront resource.
--
--
module Network.AWS.CloudFront.UntagResource
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

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to remove tags from a CloudFront resource.
--
--
--
-- /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urResource :: !Text
  , _urTagKeys  :: !TagKeys
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResource' - An ARN of a CloudFront resource.
--
-- * 'urTagKeys' - A complex type that contains zero or more @Tag@ key elements.
untagResource
    :: Text -- ^ 'urResource'
    -> TagKeys -- ^ 'urTagKeys'
    -> UntagResource
untagResource pResource_ pTagKeys_ =
  UntagResource' {_urResource = pResource_, _urTagKeys = pTagKeys_}


-- | An ARN of a CloudFront resource.
urResource :: Lens' UntagResource Text
urResource = lens _urResource (\ s a -> s{_urResource = a})

-- | A complex type that contains zero or more @Tag@ key elements.
urTagKeys :: Lens' UntagResource TagKeys
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a})

instance AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        request = postXML cloudFront
        response = receiveNull UntagResourceResponse'

instance Hashable UntagResource where

instance NFData UntagResource where

instance ToElement UntagResource where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}TagKeys"
              .
              _urTagKeys

instance ToHeaders UntagResource where
        toHeaders = const mempty

instance ToPath UntagResource where
        toPath = const "/2017-10-30/tagging"

instance ToQuery UntagResource where
        toQuery UntagResource'{..}
          = mconcat
              ["Resource" =: _urResource, "Operation=Untag"]

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
