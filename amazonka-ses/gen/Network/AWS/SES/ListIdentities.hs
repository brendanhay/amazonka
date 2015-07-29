{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListIdentities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list containing all of the identities (email addresses and
-- domains) for a specific AWS Account, regardless of verification status.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_ListIdentities.html>
module Network.AWS.SES.ListIdentities
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , listIdentities
    -- ** Request lenses
    , liIdentityType
    , liNextToken
    , liMaxItems

    -- * Response
    , ListIdentitiesResponse
    -- ** Response constructor
    , listIdentitiesResponse
    -- ** Response lenses
    , lirsNextToken
    , lirsStatus
    , lirsIdentities
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to list all identities for
-- the AWS Account.
--
-- /See:/ 'listIdentities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liIdentityType'
--
-- * 'liNextToken'
--
-- * 'liMaxItems'
data ListIdentities = ListIdentities'
    { _liIdentityType :: !(Maybe IdentityType)
    , _liNextToken    :: !(Maybe Text)
    , _liMaxItems     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentities' smart constructor.
listIdentities :: ListIdentities
listIdentities =
    ListIdentities'
    { _liIdentityType = Nothing
    , _liNextToken = Nothing
    , _liMaxItems = Nothing
    }

-- | The type of the identities to list. Possible values are \"EmailAddress\"
-- and \"Domain\". If this parameter is omitted, then all identities will
-- be listed.
liIdentityType :: Lens' ListIdentities (Maybe IdentityType)
liIdentityType = lens _liIdentityType (\ s a -> s{_liIdentityType = a});

-- | The token to use for pagination.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a});

-- | The maximum number of identities per page. Possible values are 1-1000
-- inclusive.
liMaxItems :: Lens' ListIdentities (Maybe Int)
liMaxItems = lens _liMaxItems (\ s a -> s{_liMaxItems = a});

instance AWSPager ListIdentities where
        page rq rs
          | stop (rs ^. lirsNextToken) = Nothing
          | stop (rs ^. lirsIdentities) = Nothing
          | otherwise =
            Just $ rq & liNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListIdentities where
        type Sv ListIdentities = SES
        type Rs ListIdentities = ListIdentitiesResponse
        request = postQuery
        response
          = receiveXMLWrapper "ListIdentitiesResult"
              (\ s h x ->
                 ListIdentitiesResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "Identities" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListIdentities where
        toHeaders = const mempty

instance ToPath ListIdentities where
        toPath = const mempty

instance ToQuery ListIdentities where
        toQuery ListIdentities'{..}
          = mconcat
              ["Action" =: ("ListIdentities" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "IdentityType" =: _liIdentityType,
               "NextToken" =: _liNextToken,
               "MaxItems" =: _liMaxItems]

-- | Represents a list of all verified identities for the AWS Account.
--
-- /See:/ 'listIdentitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirsNextToken'
--
-- * 'lirsStatus'
--
-- * 'lirsIdentities'
data ListIdentitiesResponse = ListIdentitiesResponse'
    { _lirsNextToken  :: !(Maybe Text)
    , _lirsStatus     :: !Int
    , _lirsIdentities :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentitiesResponse' smart constructor.
listIdentitiesResponse :: Int -> ListIdentitiesResponse
listIdentitiesResponse pStatus_ =
    ListIdentitiesResponse'
    { _lirsNextToken = Nothing
    , _lirsStatus = pStatus_
    , _lirsIdentities = mempty
    }

-- | The token used for pagination.
lirsNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a});

-- | FIXME: Undocumented member.
lirsStatus :: Lens' ListIdentitiesResponse Int
lirsStatus = lens _lirsStatus (\ s a -> s{_lirsStatus = a});

-- | A list of identities.
lirsIdentities :: Lens' ListIdentitiesResponse [Text]
lirsIdentities = lens _lirsIdentities (\ s a -> s{_lirsIdentities = a}) . _Coerce;
