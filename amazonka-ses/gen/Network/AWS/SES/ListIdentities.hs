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
    , lirqIdentityType
    , lirqNextToken
    , lirqMaxItems

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
-- * 'lirqIdentityType'
--
-- * 'lirqNextToken'
--
-- * 'lirqMaxItems'
data ListIdentities = ListIdentities'
    { _lirqIdentityType :: !(Maybe IdentityType)
    , _lirqNextToken    :: !(Maybe Text)
    , _lirqMaxItems     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentities' smart constructor.
listIdentities :: ListIdentities
listIdentities =
    ListIdentities'
    { _lirqIdentityType = Nothing
    , _lirqNextToken = Nothing
    , _lirqMaxItems = Nothing
    }

-- | The type of the identities to list. Possible values are \"EmailAddress\"
-- and \"Domain\". If this parameter is omitted, then all identities will
-- be listed.
lirqIdentityType :: Lens' ListIdentities (Maybe IdentityType)
lirqIdentityType = lens _lirqIdentityType (\ s a -> s{_lirqIdentityType = a});

-- | The token to use for pagination.
lirqNextToken :: Lens' ListIdentities (Maybe Text)
lirqNextToken = lens _lirqNextToken (\ s a -> s{_lirqNextToken = a});

-- | The maximum number of identities per page. Possible values are 1-1000
-- inclusive.
lirqMaxItems :: Lens' ListIdentities (Maybe Int)
lirqMaxItems = lens _lirqMaxItems (\ s a -> s{_lirqMaxItems = a});

instance AWSPager ListIdentities where
        page rq rs
          | stop (rs ^. lirsNextToken) = Nothing
          | stop (rs ^. lirsIdentities) = Nothing
          | otherwise =
            Just $ rq & lirqNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListIdentities where
        type Sv ListIdentities = SES
        type Rs ListIdentities = ListIdentitiesResponse
        request = post
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
        toPath = const "/"

instance ToQuery ListIdentities where
        toQuery ListIdentities'{..}
          = mconcat
              ["Action" =: ("ListIdentities" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "IdentityType" =: _lirqIdentityType,
               "NextToken" =: _lirqNextToken,
               "MaxItems" =: _lirqMaxItems]

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
listIdentitiesResponse pStatus =
    ListIdentitiesResponse'
    { _lirsNextToken = Nothing
    , _lirsStatus = pStatus
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
lirsIdentities = lens _lirsIdentities (\ s a -> s{_lirsIdentities = a});
