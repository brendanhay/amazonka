{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SSM.ListAssociations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the associations for the specified configuration document or instance.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_ListAssociations.html>
module Network.AWS.SSM.ListAssociations
    (
    -- * Request
      ListAssociations
    -- ** Request constructor
    , listAssociations
    -- ** Request lenses
    , laAssociationFilterList
    , laMaxResults
    , laNextToken

    -- * Response
    , ListAssociationsResponse
    -- ** Response constructor
    , listAssociationsResponse
    -- ** Response lenses
    , larAssociations
    , larNextToken
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

data ListAssociations = ListAssociations
    { _laAssociationFilterList :: List1 "AssociationFilter" AssociationFilter
    , _laMaxResults            :: Maybe Nat
    , _laNextToken             :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListAssociations' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laAssociationFilterList' @::@ 'NonEmpty' 'AssociationFilter'
--
-- * 'laMaxResults' @::@ 'Maybe' 'Natural'
--
-- * 'laNextToken' @::@ 'Maybe' 'Text'
--
listAssociations :: NonEmpty AssociationFilter -- ^ 'laAssociationFilterList'
                 -> ListAssociations
listAssociations p1 = ListAssociations
    { _laAssociationFilterList = withIso _List1 (const id) p1
    , _laMaxResults            = Nothing
    , _laNextToken             = Nothing
    }

-- | One or more filters. Use a filter to return a more specific list of results.
laAssociationFilterList :: Lens' ListAssociations (NonEmpty AssociationFilter)
laAssociationFilterList =
    lens _laAssociationFilterList (\s a -> s { _laAssociationFilterList = a })
        . _List1

-- | The maximum number of items to return for this call. The call also returns a
-- token that you can specify in a subsequent call to get the next set of
-- results.
laMaxResults :: Lens' ListAssociations (Maybe Natural)
laMaxResults = lens _laMaxResults (\s a -> s { _laMaxResults = a }) . mapping _Nat

-- | The token for the next set of items to return. (You received this token from
-- a previous call.)
laNextToken :: Lens' ListAssociations (Maybe Text)
laNextToken = lens _laNextToken (\s a -> s { _laNextToken = a })

data ListAssociationsResponse = ListAssociationsResponse
    { _larAssociations :: List "Association" Association
    , _larNextToken    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListAssociationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larAssociations' @::@ ['Association']
--
-- * 'larNextToken' @::@ 'Maybe' 'Text'
--
listAssociationsResponse :: ListAssociationsResponse
listAssociationsResponse = ListAssociationsResponse
    { _larAssociations = mempty
    , _larNextToken    = Nothing
    }

-- | The associations.
larAssociations :: Lens' ListAssociationsResponse [Association]
larAssociations = lens _larAssociations (\s a -> s { _larAssociations = a }) . _List

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
larNextToken :: Lens' ListAssociationsResponse (Maybe Text)
larNextToken = lens _larNextToken (\s a -> s { _larNextToken = a })

instance ToPath ListAssociations where
    toPath = const "/"

instance ToQuery ListAssociations where
    toQuery = const mempty

instance ToHeaders ListAssociations

instance ToJSON ListAssociations where
    toJSON ListAssociations{..} = object
        [ "AssociationFilterList" .= _laAssociationFilterList
        , "MaxResults"            .= _laMaxResults
        , "NextToken"             .= _laNextToken
        ]

instance AWSRequest ListAssociations where
    type Sv ListAssociations = SSM
    type Rs ListAssociations = ListAssociationsResponse

    request  = post "ListAssociations"
    response = jsonResponse

instance FromJSON ListAssociationsResponse where
    parseJSON = withObject "ListAssociationsResponse" $ \o -> ListAssociationsResponse
        <$> o .:? "Associations" .!= mempty
        <*> o .:? "NextToken"
