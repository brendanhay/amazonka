{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SSM.ListAssociations
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the associations for the specified configuration document or
-- instance.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_ListAssociations.html>
module Network.AWS.SSM.ListAssociations
    (
    -- * Request
      ListAssociations
    -- ** Request constructor
    , listAssociations
    -- ** Request lenses
    , laNextToken
    , laMaxResults
    , laAssociationFilterList

    -- * Response
    , ListAssociationsResponse
    -- ** Response constructor
    , listAssociationsResponse
    -- ** Response lenses
    , larNextToken
    , larAssociations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'listAssociations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laNextToken'
--
-- * 'laMaxResults'
--
-- * 'laAssociationFilterList'
data ListAssociations = ListAssociations'{_laNextToken :: Maybe Text, _laMaxResults :: Maybe Nat, _laAssociationFilterList :: List1 AssociationFilter} deriving (Eq, Read, Show)

-- | 'ListAssociations' smart constructor.
listAssociations :: NonEmpty AssociationFilter -> ListAssociations
listAssociations pAssociationFilterList = ListAssociations'{_laNextToken = Nothing, _laMaxResults = Nothing, _laAssociationFilterList = _List1 # pAssociationFilterList};

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
laNextToken :: Lens' ListAssociations (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a});

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
laMaxResults :: Lens' ListAssociations (Maybe Natural)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a}) . mapping _Nat;

-- | One or more filters. Use a filter to return a more specific list of
-- results.
laAssociationFilterList :: Lens' ListAssociations (NonEmpty AssociationFilter)
laAssociationFilterList = lens _laAssociationFilterList (\ s a -> s{_laAssociationFilterList = a}) . _List1;

instance AWSRequest ListAssociations where
        type Sv ListAssociations = SSM
        type Rs ListAssociations = ListAssociationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListAssociationsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Associations" .!@ mempty))

instance ToHeaders ListAssociations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListAssociations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssociations where
        toJSON ListAssociations'{..}
          = object
              ["NextToken" .= _laNextToken,
               "MaxResults" .= _laMaxResults,
               "AssociationFilterList" .= _laAssociationFilterList]

instance ToPath ListAssociations where
        toPath = const "/"

instance ToQuery ListAssociations where
        toQuery = const mempty

-- | /See:/ 'listAssociationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larNextToken'
--
-- * 'larAssociations'
data ListAssociationsResponse = ListAssociationsResponse'{_larNextToken :: Maybe Text, _larAssociations :: Maybe [Association]} deriving (Eq, Read, Show)

-- | 'ListAssociationsResponse' smart constructor.
listAssociationsResponse :: ListAssociationsResponse
listAssociationsResponse = ListAssociationsResponse'{_larNextToken = Nothing, _larAssociations = Nothing};

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
larNextToken :: Lens' ListAssociationsResponse (Maybe Text)
larNextToken = lens _larNextToken (\ s a -> s{_larNextToken = a});

-- | The associations.
larAssociations :: Lens' ListAssociationsResponse [Association]
larAssociations = lens _larAssociations (\ s a -> s{_larAssociations = a}) . _Default;
