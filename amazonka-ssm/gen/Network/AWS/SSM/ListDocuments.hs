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

-- Module      : Network.AWS.SSM.ListDocuments
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

-- | Describes one or more of your configuration documents.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_ListDocuments.html>
module Network.AWS.SSM.ListDocuments
    (
    -- * Request
      ListDocuments
    -- ** Request constructor
    , listDocuments
    -- ** Request lenses
    , ldDocumentFilterList
    , ldMaxResults
    , ldNextToken

    -- * Response
    , ListDocumentsResponse
    -- ** Response constructor
    , listDocumentsResponse
    -- ** Response lenses
    , ldrDocumentIdentifiers
    , ldrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

data ListDocuments = ListDocuments
    { _ldDocumentFilterList :: List1 "DocumentFilter" DocumentFilter
    , _ldMaxResults         :: Maybe Nat
    , _ldNextToken          :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListDocuments' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldDocumentFilterList' @::@ 'NonEmpty' 'DocumentFilter'
--
-- * 'ldMaxResults' @::@ 'Maybe' 'Natural'
--
-- * 'ldNextToken' @::@ 'Maybe' 'Text'
--
listDocuments :: NonEmpty DocumentFilter -- ^ 'ldDocumentFilterList'
              -> ListDocuments
listDocuments p1 = ListDocuments
    { _ldDocumentFilterList = withIso _List1 (const id) p1
    , _ldMaxResults         = Nothing
    , _ldNextToken          = Nothing
    }

-- | One or more filters. Use a filter to return a more specific list of results.
ldDocumentFilterList :: Lens' ListDocuments (NonEmpty DocumentFilter)
ldDocumentFilterList =
    lens _ldDocumentFilterList (\s a -> s { _ldDocumentFilterList = a })
        . _List1

-- | The maximum number of items to return for this call. The call also returns a
-- token that you can specify in a subsequent call to get the next set of
-- results.
ldMaxResults :: Lens' ListDocuments (Maybe Natural)
ldMaxResults = lens _ldMaxResults (\s a -> s { _ldMaxResults = a }) . mapping _Nat

-- | The token for the next set of items to return. (You received this token from
-- a previous call.)
ldNextToken :: Lens' ListDocuments (Maybe Text)
ldNextToken = lens _ldNextToken (\s a -> s { _ldNextToken = a })

data ListDocumentsResponse = ListDocumentsResponse
    { _ldrDocumentIdentifiers :: List "DocumentIdentifier" DocumentIdentifier
    , _ldrNextToken           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListDocumentsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDocumentIdentifiers' @::@ ['DocumentIdentifier']
--
-- * 'ldrNextToken' @::@ 'Maybe' 'Text'
--
listDocumentsResponse :: ListDocumentsResponse
listDocumentsResponse = ListDocumentsResponse
    { _ldrDocumentIdentifiers = mempty
    , _ldrNextToken           = Nothing
    }

-- | The names of the configuration documents.
ldrDocumentIdentifiers :: Lens' ListDocumentsResponse [DocumentIdentifier]
ldrDocumentIdentifiers =
    lens _ldrDocumentIdentifiers (\s a -> s { _ldrDocumentIdentifiers = a })
        . _List

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
ldrNextToken :: Lens' ListDocumentsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\s a -> s { _ldrNextToken = a })

instance ToPath ListDocuments where
    toPath = const "/"

instance ToQuery ListDocuments where
    toQuery = const mempty

instance ToHeaders ListDocuments

instance ToJSON ListDocuments where
    toJSON ListDocuments{..} = object
        [ "DocumentFilterList" .= _ldDocumentFilterList
        , "MaxResults"         .= _ldMaxResults
        , "NextToken"          .= _ldNextToken
        ]

instance AWSRequest ListDocuments where
    type Sv ListDocuments = SSM
    type Rs ListDocuments = ListDocumentsResponse

    request  = post "ListDocuments"
    response = jsonResponse

instance FromJSON ListDocumentsResponse where
    parseJSON = withObject "ListDocumentsResponse" $ \o -> ListDocumentsResponse
        <$> o .:? "DocumentIdentifiers" .!= mempty
        <*> o .:? "NextToken"
