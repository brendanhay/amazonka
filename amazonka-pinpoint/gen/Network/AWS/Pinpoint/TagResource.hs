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
-- Module      : Network.AWS.Pinpoint.TagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.Pinpoint.TagResource
    (
    -- * Creating a Request
      tagResource
    , TagResource
    -- * Request Lenses
    , trResourceARN
    , trTagsModel

    -- * Destructuring the Response
    , tagResourceResponse
    , TagResourceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceARN :: !Text
  , _trTagsModel   :: !TagsModel
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - Undocumented member.
--
-- * 'trTagsModel' - Undocumented member.
tagResource
    :: Text -- ^ 'trResourceARN'
    -> TagsModel -- ^ 'trTagsModel'
    -> TagResource
tagResource pResourceARN_ pTagsModel_ =
  TagResource' {_trResourceARN = pResourceARN_, _trTagsModel = pTagsModel_}


-- | Undocumented member.
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\ s a -> s{_trResourceARN = a})

-- | Undocumented member.
trTagsModel :: Lens' TagResource TagsModel
trTagsModel = lens _trTagsModel (\ s a -> s{_trTagsModel = a})

instance AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        request = postJSON pinpoint
        response = receiveNull TagResourceResponse'

instance Hashable TagResource where

instance NFData TagResource where

instance ToHeaders TagResource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagResource where
        toJSON TagResource'{..}
          = object
              (catMaybes [Just ("TagsModel" .= _trTagsModel)])

instance ToPath TagResource where
        toPath TagResource'{..}
          = mconcat ["/v1/tags/", toBS _trResourceARN]

instance ToQuery TagResource where
        toQuery = const mempty

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
