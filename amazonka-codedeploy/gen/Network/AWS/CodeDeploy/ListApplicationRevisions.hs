{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListApplicationRevisions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists information about revisions for an application.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListApplicationRevisions.html>
module Network.AWS.CodeDeploy.ListApplicationRevisions
    (
    -- * Request
      ListApplicationRevisions
    -- ** Request constructor
    , listApplicationRevisions
    -- ** Request lenses
    , larrqS3KeyPrefix
    , larrqDeployed
    , larrqNextToken
    , larrqSortOrder
    , larrqS3Bucket
    , larrqSortBy
    , larrqApplicationName

    -- * Response
    , ListApplicationRevisionsResponse
    -- ** Response constructor
    , listApplicationRevisionsResponse
    -- ** Response lenses
    , larrsNextToken
    , larrsRevisions
    , larrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list application revisions operation.
--
-- /See:/ 'listApplicationRevisions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larrqS3KeyPrefix'
--
-- * 'larrqDeployed'
--
-- * 'larrqNextToken'
--
-- * 'larrqSortOrder'
--
-- * 'larrqS3Bucket'
--
-- * 'larrqSortBy'
--
-- * 'larrqApplicationName'
data ListApplicationRevisions = ListApplicationRevisions'
    { _larrqS3KeyPrefix     :: !(Maybe Text)
    , _larrqDeployed        :: !(Maybe ListStateFilterAction)
    , _larrqNextToken       :: !(Maybe Text)
    , _larrqSortOrder       :: !(Maybe SortOrder)
    , _larrqS3Bucket        :: !(Maybe Text)
    , _larrqSortBy          :: !(Maybe ApplicationRevisionSortBy)
    , _larrqApplicationName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListApplicationRevisions' smart constructor.
listApplicationRevisions :: Text -> ListApplicationRevisions
listApplicationRevisions pApplicationName_ =
    ListApplicationRevisions'
    { _larrqS3KeyPrefix = Nothing
    , _larrqDeployed = Nothing
    , _larrqNextToken = Nothing
    , _larrqSortOrder = Nothing
    , _larrqS3Bucket = Nothing
    , _larrqSortBy = Nothing
    , _larrqApplicationName = pApplicationName_
    }

-- | A specific key prefix for the set of Amazon S3 objects to limit the
-- search for revisions.
larrqS3KeyPrefix :: Lens' ListApplicationRevisions (Maybe Text)
larrqS3KeyPrefix = lens _larrqS3KeyPrefix (\ s a -> s{_larrqS3KeyPrefix = a});

-- | Whether to list revisions based on whether the revision is the target
-- revision of an deployment group:
--
-- -   include: List revisions that are target revisions of a deployment
--     group.
-- -   exclude: Do not list revisions that are target revisions of a
--     deployment group.
-- -   ignore: List all revisions, regardless of whether they are target
--     revisions of a deployment group.
larrqDeployed :: Lens' ListApplicationRevisions (Maybe ListStateFilterAction)
larrqDeployed = lens _larrqDeployed (\ s a -> s{_larrqDeployed = a});

-- | An identifier that was returned from the previous list application
-- revisions call, which can be used to return the next set of applications
-- in the list.
larrqNextToken :: Lens' ListApplicationRevisions (Maybe Text)
larrqNextToken = lens _larrqNextToken (\ s a -> s{_larrqNextToken = a});

-- | The order to sort the list results by:
--
-- -   ascending: Sort the list of results in ascending order.
-- -   descending: Sort the list of results in descending order.
--
-- If not specified, the results will be sorted in ascending order.
--
-- If set to null, the results will be sorted in an arbitrary order.
larrqSortOrder :: Lens' ListApplicationRevisions (Maybe SortOrder)
larrqSortOrder = lens _larrqSortOrder (\ s a -> s{_larrqSortOrder = a});

-- | A specific Amazon S3 bucket name to limit the search for revisions.
--
-- If set to null, then all of the user\'s buckets will be searched.
larrqS3Bucket :: Lens' ListApplicationRevisions (Maybe Text)
larrqS3Bucket = lens _larrqS3Bucket (\ s a -> s{_larrqS3Bucket = a});

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
larrqSortBy :: Lens' ListApplicationRevisions (Maybe ApplicationRevisionSortBy)
larrqSortBy = lens _larrqSortBy (\ s a -> s{_larrqSortBy = a});

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
larrqApplicationName :: Lens' ListApplicationRevisions Text
larrqApplicationName = lens _larrqApplicationName (\ s a -> s{_larrqApplicationName = a});

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
                     (x .?> "revisions" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["s3KeyPrefix" .= _larrqS3KeyPrefix,
               "deployed" .= _larrqDeployed,
               "nextToken" .= _larrqNextToken,
               "sortOrder" .= _larrqSortOrder,
               "s3Bucket" .= _larrqS3Bucket,
               "sortBy" .= _larrqSortBy,
               "applicationName" .= _larrqApplicationName]

instance ToPath ListApplicationRevisions where
        toPath = const "/"

instance ToQuery ListApplicationRevisions where
        toQuery = const mempty

-- | Represents the output of a list application revisions operation.
--
-- /See:/ 'listApplicationRevisionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larrsNextToken'
--
-- * 'larrsRevisions'
--
-- * 'larrsStatus'
data ListApplicationRevisionsResponse = ListApplicationRevisionsResponse'
    { _larrsNextToken :: !(Maybe Text)
    , _larrsRevisions :: !(Maybe [RevisionLocation])
    , _larrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListApplicationRevisionsResponse' smart constructor.
listApplicationRevisionsResponse :: Int -> ListApplicationRevisionsResponse
listApplicationRevisionsResponse pStatus_ =
    ListApplicationRevisionsResponse'
    { _larrsNextToken = Nothing
    , _larrsRevisions = Nothing
    , _larrsStatus = pStatus_
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- application revisions call to return the next set of application
-- revisions in the list.
larrsNextToken :: Lens' ListApplicationRevisionsResponse (Maybe Text)
larrsNextToken = lens _larrsNextToken (\ s a -> s{_larrsNextToken = a});

-- | A list of revision locations that contain the matching revisions.
larrsRevisions :: Lens' ListApplicationRevisionsResponse [RevisionLocation]
larrsRevisions = lens _larrsRevisions (\ s a -> s{_larrsRevisions = a}) . _Default;

-- | FIXME: Undocumented member.
larrsStatus :: Lens' ListApplicationRevisionsResponse Int
larrsStatus = lens _larrsStatus (\ s a -> s{_larrsStatus = a});
