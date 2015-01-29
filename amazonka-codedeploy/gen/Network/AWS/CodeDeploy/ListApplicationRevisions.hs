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

-- Module      : Network.AWS.CodeDeploy.ListApplicationRevisions
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

-- | Lists information about revisions for an application.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListApplicationRevisions.html>
module Network.AWS.CodeDeploy.ListApplicationRevisions
    (
    -- * Request
      ListApplicationRevisions
    -- ** Request constructor
    , listApplicationRevisions
    -- ** Request lenses
    , larApplicationName
    , larDeployed
    , larNextToken
    , larS3Bucket
    , larS3KeyPrefix
    , larSortBy
    , larSortOrder

    -- * Response
    , ListApplicationRevisionsResponse
    -- ** Response constructor
    , listApplicationRevisionsResponse
    -- ** Response lenses
    , larrNextToken
    , larrRevisions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data ListApplicationRevisions = ListApplicationRevisions
    { _larApplicationName :: Text
    , _larDeployed        :: Maybe ListStateFilterAction
    , _larNextToken       :: Maybe Text
    , _larS3Bucket        :: Maybe Text
    , _larS3KeyPrefix     :: Maybe Text
    , _larSortBy          :: Maybe ApplicationRevisionSortBy
    , _larSortOrder       :: Maybe SortOrder
    } deriving (Eq, Read, Show)

-- | 'ListApplicationRevisions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larApplicationName' @::@ 'Text'
--
-- * 'larDeployed' @::@ 'Maybe' 'ListStateFilterAction'
--
-- * 'larNextToken' @::@ 'Maybe' 'Text'
--
-- * 'larS3Bucket' @::@ 'Maybe' 'Text'
--
-- * 'larS3KeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'larSortBy' @::@ 'Maybe' 'ApplicationRevisionSortBy'
--
-- * 'larSortOrder' @::@ 'Maybe' 'SortOrder'
--
listApplicationRevisions :: Text -- ^ 'larApplicationName'
                         -> ListApplicationRevisions
listApplicationRevisions p1 = ListApplicationRevisions
    { _larApplicationName = p1
    , _larSortBy          = Nothing
    , _larSortOrder       = Nothing
    , _larS3Bucket        = Nothing
    , _larS3KeyPrefix     = Nothing
    , _larDeployed        = Nothing
    , _larNextToken       = Nothing
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
larApplicationName :: Lens' ListApplicationRevisions Text
larApplicationName =
    lens _larApplicationName (\s a -> s { _larApplicationName = a })

-- | Whether to list revisions based on whether the revision is the target
-- revision of an deployment group:
--
-- include: List revisions that are target revisions of a deployment group. exclude: Do not list revisions that are target revisions of a deployment group.
-- ignore: List all revisions, regardless of whether they are target revisions
-- of a deployment group.
larDeployed :: Lens' ListApplicationRevisions (Maybe ListStateFilterAction)
larDeployed = lens _larDeployed (\s a -> s { _larDeployed = a })

-- | An identifier that was returned from the previous list application revisions
-- call, which can be used to return the next set of applications in the list.
larNextToken :: Lens' ListApplicationRevisions (Maybe Text)
larNextToken = lens _larNextToken (\s a -> s { _larNextToken = a })

-- | A specific Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, then all of the user's buckets will be searched.
larS3Bucket :: Lens' ListApplicationRevisions (Maybe Text)
larS3Bucket = lens _larS3Bucket (\s a -> s { _larS3Bucket = a })

-- | A specific key prefix for the set of Amazon S3 objects to limit the search
-- for revisions.
larS3KeyPrefix :: Lens' ListApplicationRevisions (Maybe Text)
larS3KeyPrefix = lens _larS3KeyPrefix (\s a -> s { _larS3KeyPrefix = a })

-- | The column name to sort the list results by:
--
-- registerTime: Sort the list results by when the revisions were registered
-- with AWS CodeDeploy. firstUsedTime: Sort the list results by when the
-- revisions were first used by in a deployment. lastUsedTime: Sort the list
-- results by when the revisions were last used in a deployment.  If not
-- specified or set to null, the results will be returned in an arbitrary order.
larSortBy :: Lens' ListApplicationRevisions (Maybe ApplicationRevisionSortBy)
larSortBy = lens _larSortBy (\s a -> s { _larSortBy = a })

-- | The order to sort the list results by:
--
-- ascending: Sort the list results in ascending order. descending: Sort the
-- list results in descending order.  If not specified, the results will be
-- sorted in ascending order.
--
-- If set to null, the results will be sorted in an arbitrary order.
larSortOrder :: Lens' ListApplicationRevisions (Maybe SortOrder)
larSortOrder = lens _larSortOrder (\s a -> s { _larSortOrder = a })

data ListApplicationRevisionsResponse = ListApplicationRevisionsResponse
    { _larrNextToken :: Maybe Text
    , _larrRevisions :: List "revisions" RevisionLocation
    } deriving (Eq, Read, Show)

-- | 'ListApplicationRevisionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'larrRevisions' @::@ ['RevisionLocation']
--
listApplicationRevisionsResponse :: ListApplicationRevisionsResponse
listApplicationRevisionsResponse = ListApplicationRevisionsResponse
    { _larrRevisions = mempty
    , _larrNextToken = Nothing
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- application revisions call to return the next set of application revisions in
-- the list.
larrNextToken :: Lens' ListApplicationRevisionsResponse (Maybe Text)
larrNextToken = lens _larrNextToken (\s a -> s { _larrNextToken = a })

-- | A list of revision locations that contain the matching revisions.
larrRevisions :: Lens' ListApplicationRevisionsResponse [RevisionLocation]
larrRevisions = lens _larrRevisions (\s a -> s { _larrRevisions = a }) . _List

instance ToPath ListApplicationRevisions where
    toPath = const "/"

instance ToQuery ListApplicationRevisions where
    toQuery = const mempty

instance ToHeaders ListApplicationRevisions

instance ToJSON ListApplicationRevisions where
    toJSON ListApplicationRevisions{..} = object
        [ "applicationName" .= _larApplicationName
        , "sortBy"          .= _larSortBy
        , "sortOrder"       .= _larSortOrder
        , "s3Bucket"        .= _larS3Bucket
        , "s3KeyPrefix"     .= _larS3KeyPrefix
        , "deployed"        .= _larDeployed
        , "nextToken"       .= _larNextToken
        ]

instance AWSRequest ListApplicationRevisions where
    type Sv ListApplicationRevisions = CodeDeploy
    type Rs ListApplicationRevisions = ListApplicationRevisionsResponse

    request  = post "ListApplicationRevisions"
    response = jsonResponse

instance FromJSON ListApplicationRevisionsResponse where
    parseJSON = withObject "ListApplicationRevisionsResponse" $ \o -> ListApplicationRevisionsResponse
        <$> o .:? "nextToken"
        <*> o .:? "revisions" .!= mempty
