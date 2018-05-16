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
-- Module      : Network.AWS.Route53Domains.ListTagsForDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all of the tags that are associated with the specified domain.
--
--
-- All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.
--
module Network.AWS.Route53Domains.ListTagsForDomain
    (
    -- * Creating a Request
      listTagsForDomain
    , ListTagsForDomain
    -- * Request Lenses
    , ltfdDomainName

    -- * Destructuring the Response
    , listTagsForDomainResponse
    , ListTagsForDomainResponse
    -- * Response Lenses
    , ltfdrsResponseStatus
    , ltfdrsTagList
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The ListTagsForDomainRequest includes the following elements.
--
--
--
-- /See:/ 'listTagsForDomain' smart constructor.
newtype ListTagsForDomain = ListTagsForDomain'
  { _ltfdDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfdDomainName' - The domain for which you want to get a list of tags.
listTagsForDomain
    :: Text -- ^ 'ltfdDomainName'
    -> ListTagsForDomain
listTagsForDomain pDomainName_ =
  ListTagsForDomain' {_ltfdDomainName = pDomainName_}


-- | The domain for which you want to get a list of tags.
ltfdDomainName :: Lens' ListTagsForDomain Text
ltfdDomainName = lens _ltfdDomainName (\ s a -> s{_ltfdDomainName = a})

instance AWSRequest ListTagsForDomain where
        type Rs ListTagsForDomain = ListTagsForDomainResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .?> "TagList" .!@ mempty))

instance Hashable ListTagsForDomain where

instance NFData ListTagsForDomain where

instance ToHeaders ListTagsForDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.ListTagsForDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForDomain where
        toJSON ListTagsForDomain'{..}
          = object
              (catMaybes [Just ("DomainName" .= _ltfdDomainName)])

instance ToPath ListTagsForDomain where
        toPath = const "/"

instance ToQuery ListTagsForDomain where
        toQuery = const mempty

-- | The ListTagsForDomain response includes the following elements.
--
--
--
-- /See:/ 'listTagsForDomainResponse' smart constructor.
data ListTagsForDomainResponse = ListTagsForDomainResponse'
  { _ltfdrsResponseStatus :: !Int
  , _ltfdrsTagList        :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfdrsResponseStatus' - -- | The response status code.
--
-- * 'ltfdrsTagList' - A list of the tags that are associated with the specified domain.
listTagsForDomainResponse
    :: Int -- ^ 'ltfdrsResponseStatus'
    -> ListTagsForDomainResponse
listTagsForDomainResponse pResponseStatus_ =
  ListTagsForDomainResponse'
    {_ltfdrsResponseStatus = pResponseStatus_, _ltfdrsTagList = mempty}


-- | -- | The response status code.
ltfdrsResponseStatus :: Lens' ListTagsForDomainResponse Int
ltfdrsResponseStatus = lens _ltfdrsResponseStatus (\ s a -> s{_ltfdrsResponseStatus = a})

-- | A list of the tags that are associated with the specified domain.
ltfdrsTagList :: Lens' ListTagsForDomainResponse [Tag]
ltfdrsTagList = lens _ltfdrsTagList (\ s a -> s{_ltfdrsTagList = a}) . _Coerce

instance NFData ListTagsForDomainResponse where
