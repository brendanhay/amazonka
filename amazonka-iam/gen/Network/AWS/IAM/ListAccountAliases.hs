{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAccountAliases
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the account aliases associated with the account. For information
-- about using an AWS account alias, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccountAliases.html>
module Network.AWS.IAM.ListAccountAliases
    (
    -- * Request
      ListAccountAliases
    -- ** Request constructor
    , listAccountAliases
    -- ** Request lenses
    , laarqMaxItems
    , laarqMarker

    -- * Response
    , ListAccountAliasesResponse
    -- ** Response constructor
    , listAccountAliasesResponse
    -- ** Response lenses
    , laarsMarker
    , laarsIsTruncated
    , laarsStatus
    , laarsAccountAliases
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAccountAliases' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laarqMaxItems'
--
-- * 'laarqMarker'
data ListAccountAliases = ListAccountAliases'
    { _laarqMaxItems :: !(Maybe Nat)
    , _laarqMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAccountAliases' smart constructor.
listAccountAliases :: ListAccountAliases
listAccountAliases =
    ListAccountAliases'
    { _laarqMaxItems = Nothing
    , _laarqMarker = Nothing
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
laarqMaxItems :: Lens' ListAccountAliases (Maybe Natural)
laarqMaxItems = lens _laarqMaxItems (\ s a -> s{_laarqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
laarqMarker :: Lens' ListAccountAliases (Maybe Text)
laarqMarker = lens _laarqMarker (\ s a -> s{_laarqMarker = a});

instance AWSPager ListAccountAliases where
        page rq rs
          | stop (rs ^. laarsIsTruncated) = Nothing
          | isNothing (rs ^. laarsMarker) = Nothing
          | otherwise =
            Just $ rq & laarqMarker .~ rs ^. laarsMarker

instance AWSRequest ListAccountAliases where
        type Sv ListAccountAliases = IAM
        type Rs ListAccountAliases =
             ListAccountAliasesResponse
        request = post
        response
          = receiveXMLWrapper "ListAccountAliasesResult"
              (\ s h x ->
                 ListAccountAliasesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "AccountAliases" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListAccountAliases where
        toHeaders = const mempty

instance ToPath ListAccountAliases where
        toPath = const "/"

instance ToQuery ListAccountAliases where
        toQuery ListAccountAliases'{..}
          = mconcat
              ["Action" =: ("ListAccountAliases" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "MaxItems" =: _laarqMaxItems,
               "Marker" =: _laarqMarker]

-- | Contains the response to a successful ListAccountAliases request.
--
-- /See:/ 'listAccountAliasesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laarsMarker'
--
-- * 'laarsIsTruncated'
--
-- * 'laarsStatus'
--
-- * 'laarsAccountAliases'
data ListAccountAliasesResponse = ListAccountAliasesResponse'
    { _laarsMarker         :: !(Maybe Text)
    , _laarsIsTruncated    :: !(Maybe Bool)
    , _laarsStatus         :: !Int
    , _laarsAccountAliases :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAccountAliasesResponse' smart constructor.
listAccountAliasesResponse :: Int -> ListAccountAliasesResponse
listAccountAliasesResponse pStatus_ =
    ListAccountAliasesResponse'
    { _laarsMarker = Nothing
    , _laarsIsTruncated = Nothing
    , _laarsStatus = pStatus_
    , _laarsAccountAliases = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
laarsMarker :: Lens' ListAccountAliasesResponse (Maybe Text)
laarsMarker = lens _laarsMarker (\ s a -> s{_laarsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
laarsIsTruncated :: Lens' ListAccountAliasesResponse (Maybe Bool)
laarsIsTruncated = lens _laarsIsTruncated (\ s a -> s{_laarsIsTruncated = a});

-- | FIXME: Undocumented member.
laarsStatus :: Lens' ListAccountAliasesResponse Int
laarsStatus = lens _laarsStatus (\ s a -> s{_laarsStatus = a});

-- | A list of aliases associated with the account.
laarsAccountAliases :: Lens' ListAccountAliasesResponse [Text]
laarsAccountAliases = lens _laarsAccountAliases (\ s a -> s{_laarsAccountAliases = a});
