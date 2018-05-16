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
-- Module      : Network.AWS.CloudFront.TagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add tags to a CloudFront resource.
--
--
module Network.AWS.CloudFront.TagResource
    (
    -- * Creating a Request
      tagResource
    , TagResource
    -- * Request Lenses
    , trResource
    , trTags

    -- * Destructuring the Response
    , tagResourceResponse
    , TagResourceResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to add tags to a CloudFront resource.
--
--
--
-- /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResource :: !Text
  , _trTags     :: !Tags
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResource' - An ARN of a CloudFront resource.
--
-- * 'trTags' - A complex type that contains zero or more @Tag@ elements.
tagResource
    :: Text -- ^ 'trResource'
    -> Tags -- ^ 'trTags'
    -> TagResource
tagResource pResource_ pTags_ =
  TagResource' {_trResource = pResource_, _trTags = pTags_}


-- | An ARN of a CloudFront resource.
trResource :: Lens' TagResource Text
trResource = lens _trResource (\ s a -> s{_trResource = a})

-- | A complex type that contains zero or more @Tag@ elements.
trTags :: Lens' TagResource Tags
trTags = lens _trTags (\ s a -> s{_trTags = a})

instance AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        request = postXML cloudFront
        response = receiveNull TagResourceResponse'

instance Hashable TagResource where

instance NFData TagResource where

instance ToElement TagResource where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}Tags"
              .
              _trTags

instance ToHeaders TagResource where
        toHeaders = const mempty

instance ToPath TagResource where
        toPath = const "/2017-10-30/tagging"

instance ToQuery TagResource where
        toQuery TagResource'{..}
          = mconcat
              ["Resource" =: _trResource, "Operation=Tag"]

-- | /See:/ 'tagResourceResponse' smart constructor.
data TagResourceResponse =
  TagResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
tagResourceResponse
    :: TagResourceResponse
tagResourceResponse = TagResourceResponse'


instance NFData TagResourceResponse where
