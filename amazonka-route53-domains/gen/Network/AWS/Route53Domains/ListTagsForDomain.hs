{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ListTagsForDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all of the tags that are associated with the
-- specified domain.
--
-- All tag operations are eventually consistent; subsequent operations may
-- not immediately represent all issued operations.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-ListTagsForDomain.html AWS API Reference> for ListTagsForDomain.
module Network.AWS.Route53Domains.ListTagsForDomain
    (
    -- * Creating a Request
      ListTagsForDomain
    , listTagsForDomain
    -- * Request Lenses
    , ltfdDomainName

    -- * Destructuring the Response
    , ListTagsForDomainResponse
    , listTagsForDomainResponse
    -- * Response Lenses
    , ltfdrsStatus
    , ltfdrsTagList
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The ListTagsForDomainRequest includes the following elements.
--
-- /See:/ 'listTagsForDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfdDomainName'
newtype ListTagsForDomain = ListTagsForDomain'
    { _ltfdDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForDomain' smart constructor.
listTagsForDomain :: Text -> ListTagsForDomain
listTagsForDomain pDomainName_ =
    ListTagsForDomain'
    { _ltfdDomainName = pDomainName_
    }

-- | The domain for which you want to get a list of tags.
ltfdDomainName :: Lens' ListTagsForDomain Text
ltfdDomainName = lens _ltfdDomainName (\ s a -> s{_ltfdDomainName = a});

instance AWSRequest ListTagsForDomain where
        type Sv ListTagsForDomain = Route53Domains
        type Rs ListTagsForDomain = ListTagsForDomainResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .?> "TagList" .!@ mempty))

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
          = object ["DomainName" .= _ltfdDomainName]

instance ToPath ListTagsForDomain where
        toPath = const "/"

instance ToQuery ListTagsForDomain where
        toQuery = const mempty

-- | The ListTagsForDomain response includes the following elements.
--
-- /See:/ 'listTagsForDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfdrsStatus'
--
-- * 'ltfdrsTagList'
data ListTagsForDomainResponse = ListTagsForDomainResponse'
    { _ltfdrsStatus  :: !Int
    , _ltfdrsTagList :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForDomainResponse' smart constructor.
listTagsForDomainResponse :: Int -> ListTagsForDomainResponse
listTagsForDomainResponse pStatus_ =
    ListTagsForDomainResponse'
    { _ltfdrsStatus = pStatus_
    , _ltfdrsTagList = mempty
    }

-- | Undocumented member.
ltfdrsStatus :: Lens' ListTagsForDomainResponse Int
ltfdrsStatus = lens _ltfdrsStatus (\ s a -> s{_ltfdrsStatus = a});

-- | A list of the tags that are associated with the specified domain.
--
-- Type: A complex type containing a list of tags
--
-- Each tag includes the following elements.
--
-- -   Key
--
--     The key (name) of a tag.
--
--     Type: String
--
-- -   Value
--
--     The value of a tag.
--
--     Type: String
--
ltfdrsTagList :: Lens' ListTagsForDomainResponse [Tag]
ltfdrsTagList = lens _ltfdrsTagList (\ s a -> s{_ltfdrsTagList = a}) . _Coerce;
