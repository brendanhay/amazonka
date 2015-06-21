{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , lisS3KeyPrefix
    , lisDeployed
    , lisNextToken
    , lisSortOrder
    , lisS3Bucket
    , lisSortBy
    , lisApplicationName

    -- * Response
    , ListApplicationRevisionsResponse
    -- ** Response constructor
    , listApplicationRevisionsResponse
    -- ** Response lenses
    , larrNextToken
    , larrRevisions
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listApplicationRevisions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisS3KeyPrefix'
--
-- * 'lisDeployed'
--
-- * 'lisNextToken'
--
-- * 'lisSortOrder'
--
-- * 'lisS3Bucket'
--
-- * 'lisSortBy'
--
-- * 'lisApplicationName'
data ListApplicationRevisions = ListApplicationRevisions'{_lisS3KeyPrefix :: Maybe Text, _lisDeployed :: Maybe ListStateFilterAction, _lisNextToken :: Maybe Text, _lisSortOrder :: Maybe SortOrder, _lisS3Bucket :: Maybe Text, _lisSortBy :: Maybe ApplicationRevisionSortBy, _lisApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'ListApplicationRevisions' smart constructor.
listApplicationRevisions :: Text -> ListApplicationRevisions
listApplicationRevisions pApplicationName = ListApplicationRevisions'{_lisS3KeyPrefix = Nothing, _lisDeployed = Nothing, _lisNextToken = Nothing, _lisSortOrder = Nothing, _lisS3Bucket = Nothing, _lisSortBy = Nothing, _lisApplicationName = pApplicationName};

-- | A specific key prefix for the set of Amazon S3 objects to limit the
-- search for revisions.
lisS3KeyPrefix :: Lens' ListApplicationRevisions (Maybe Text)
lisS3KeyPrefix = lens _lisS3KeyPrefix (\ s a -> s{_lisS3KeyPrefix = a});

-- | Whether to list revisions based on whether the revision is the target
-- revision of an deployment group:
--
-- -   include: List revisions that are target revisions of a deployment
--     group.
-- -   exclude: Do not list revisions that are target revisions of a
--     deployment group.
-- -   ignore: List all revisions, regardless of whether they are target
--     revisions of a deployment group.
lisDeployed :: Lens' ListApplicationRevisions (Maybe ListStateFilterAction)
lisDeployed = lens _lisDeployed (\ s a -> s{_lisDeployed = a});

-- | An identifier that was returned from the previous list application
-- revisions call, which can be used to return the next set of applications
-- in the list.
lisNextToken :: Lens' ListApplicationRevisions (Maybe Text)
lisNextToken = lens _lisNextToken (\ s a -> s{_lisNextToken = a});

-- | The order to sort the list results by:
--
-- -   ascending: Sort the list of results in ascending order.
-- -   descending: Sort the list of results in descending order.
--
-- If not specified, the results will be sorted in ascending order.
--
-- If set to null, the results will be sorted in an arbitrary order.
lisSortOrder :: Lens' ListApplicationRevisions (Maybe SortOrder)
lisSortOrder = lens _lisSortOrder (\ s a -> s{_lisSortOrder = a});

-- | A specific Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, then all of the user\'s buckets will be searched.
lisS3Bucket :: Lens' ListApplicationRevisions (Maybe Text)
lisS3Bucket = lens _lisS3Bucket (\ s a -> s{_lisS3Bucket = a});

-- | The column name to sort the list results by:
--
-- -   registerTime: Sort the list results by when the revisions were
--     registered with AWS CodeDeploy.
-- -   firstUsedTime: Sort the list results by when the revisions were
--     first used by in a deployment.
-- -   lastUsedTime: Sort the list results by when the revisions were last
--     used in a deployment.
--
-- If not specified or set to null, the results will be returned in an
-- arbitrary order.
lisSortBy :: Lens' ListApplicationRevisions (Maybe ApplicationRevisionSortBy)
lisSortBy = lens _lisSortBy (\ s a -> s{_lisSortBy = a});

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
lisApplicationName :: Lens' ListApplicationRevisions Text
lisApplicationName = lens _lisApplicationName (\ s a -> s{_lisApplicationName = a});

instance AWSRequest ListApplicationRevisions where
        type Sv ListApplicationRevisions = CodeDeploy
        type Rs ListApplicationRevisions =
             ListApplicationRevisionsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListApplicationRevisionsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "revisions" .!@ mempty))

instance ToHeaders ListApplicationRevisions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListApplicationRevisions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListApplicationRevisions where
        toJSON ListApplicationRevisions'{..}
          = object
              ["s3KeyPrefix" .= _lisS3KeyPrefix,
               "deployed" .= _lisDeployed,
               "nextToken" .= _lisNextToken,
               "sortOrder" .= _lisSortOrder,
               "s3Bucket" .= _lisS3Bucket, "sortBy" .= _lisSortBy,
               "applicationName" .= _lisApplicationName]

instance ToPath ListApplicationRevisions where
        toPath = const "/"

instance ToQuery ListApplicationRevisions where
        toQuery = const mempty

-- | /See:/ 'listApplicationRevisionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larrNextToken'
--
-- * 'larrRevisions'
data ListApplicationRevisionsResponse = ListApplicationRevisionsResponse'{_larrNextToken :: Maybe Text, _larrRevisions :: Maybe [RevisionLocation]} deriving (Eq, Read, Show)

-- | 'ListApplicationRevisionsResponse' smart constructor.
listApplicationRevisionsResponse :: ListApplicationRevisionsResponse
listApplicationRevisionsResponse = ListApplicationRevisionsResponse'{_larrNextToken = Nothing, _larrRevisions = Nothing};

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- application revisions call to return the next set of application
-- revisions in the list.
larrNextToken :: Lens' ListApplicationRevisionsResponse (Maybe Text)
larrNextToken = lens _larrNextToken (\ s a -> s{_larrNextToken = a});

-- | A list of revision locations that contain the matching revisions.
larrRevisions :: Lens' ListApplicationRevisionsResponse [RevisionLocation]
larrRevisions = lens _larrRevisions (\ s a -> s{_larrRevisions = a}) . _Default;
