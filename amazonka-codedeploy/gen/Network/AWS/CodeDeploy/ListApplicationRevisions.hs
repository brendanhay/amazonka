{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListApplicationRevisions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about revisions for an application.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListApplicationRevisions
    (
    -- * Creating a Request
      listApplicationRevisions
    , ListApplicationRevisions
    -- * Request Lenses
    , larS3KeyPrefix
    , larDeployed
    , larSortOrder
    , larNextToken
    , larS3Bucket
    , larSortBy
    , larApplicationName

    -- * Destructuring the Response
    , listApplicationRevisionsResponse
    , ListApplicationRevisionsResponse
    -- * Response Lenses
    , larrsNextToken
    , larrsRevisions
    , larrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListApplicationRevisions operation.
--
--
--
-- /See:/ 'listApplicationRevisions' smart constructor.
data ListApplicationRevisions = ListApplicationRevisions'
  { _larS3KeyPrefix     :: !(Maybe Text)
  , _larDeployed        :: !(Maybe ListStateFilterAction)
  , _larSortOrder       :: !(Maybe SortOrder)
  , _larNextToken       :: !(Maybe Text)
  , _larS3Bucket        :: !(Maybe Text)
  , _larSortBy          :: !(Maybe ApplicationRevisionSortBy)
  , _larApplicationName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApplicationRevisions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larS3KeyPrefix' - A key prefix for the set of Amazon S3 objects to limit the search for revisions.
--
-- * 'larDeployed' - Whether to list revisions based on whether the revision is the target revision of an deployment group:     * include: List revisions that are target revisions of a deployment group.     * exclude: Do not list revisions that are target revisions of a deployment group.     * ignore: List all revisions.
--
-- * 'larSortOrder' - The order in which to sort the list results:     * ascending: ascending order.     * descending: descending order. If not specified, the results will be sorted in ascending order. If set to null, the results will be sorted in an arbitrary order.
--
-- * 'larNextToken' - An identifier returned from the previous list application revisions call. It can be used to return the next set of applications in the list.
--
-- * 'larS3Bucket' - An Amazon S3 bucket name to limit the search for revisions. If set to null, all of the user's buckets will be searched.
--
-- * 'larSortBy' - The column name to use to sort the list results:     * registerTime: Sort by the time the revisions were registered with AWS CodeDeploy.     * firstUsedTime: Sort by the time the revisions were first used in a deployment.     * lastUsedTime: Sort by the time the revisions were last used in a deployment. If not specified or set to null, the results will be returned in an arbitrary order.
--
-- * 'larApplicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
listApplicationRevisions
    :: Text -- ^ 'larApplicationName'
    -> ListApplicationRevisions
listApplicationRevisions pApplicationName_ =
  ListApplicationRevisions'
    { _larS3KeyPrefix = Nothing
    , _larDeployed = Nothing
    , _larSortOrder = Nothing
    , _larNextToken = Nothing
    , _larS3Bucket = Nothing
    , _larSortBy = Nothing
    , _larApplicationName = pApplicationName_
    }


-- | A key prefix for the set of Amazon S3 objects to limit the search for revisions.
larS3KeyPrefix :: Lens' ListApplicationRevisions (Maybe Text)
larS3KeyPrefix = lens _larS3KeyPrefix (\ s a -> s{_larS3KeyPrefix = a})

-- | Whether to list revisions based on whether the revision is the target revision of an deployment group:     * include: List revisions that are target revisions of a deployment group.     * exclude: Do not list revisions that are target revisions of a deployment group.     * ignore: List all revisions.
larDeployed :: Lens' ListApplicationRevisions (Maybe ListStateFilterAction)
larDeployed = lens _larDeployed (\ s a -> s{_larDeployed = a})

-- | The order in which to sort the list results:     * ascending: ascending order.     * descending: descending order. If not specified, the results will be sorted in ascending order. If set to null, the results will be sorted in an arbitrary order.
larSortOrder :: Lens' ListApplicationRevisions (Maybe SortOrder)
larSortOrder = lens _larSortOrder (\ s a -> s{_larSortOrder = a})

-- | An identifier returned from the previous list application revisions call. It can be used to return the next set of applications in the list.
larNextToken :: Lens' ListApplicationRevisions (Maybe Text)
larNextToken = lens _larNextToken (\ s a -> s{_larNextToken = a})

-- | An Amazon S3 bucket name to limit the search for revisions. If set to null, all of the user's buckets will be searched.
larS3Bucket :: Lens' ListApplicationRevisions (Maybe Text)
larS3Bucket = lens _larS3Bucket (\ s a -> s{_larS3Bucket = a})

-- | The column name to use to sort the list results:     * registerTime: Sort by the time the revisions were registered with AWS CodeDeploy.     * firstUsedTime: Sort by the time the revisions were first used in a deployment.     * lastUsedTime: Sort by the time the revisions were last used in a deployment. If not specified or set to null, the results will be returned in an arbitrary order.
larSortBy :: Lens' ListApplicationRevisions (Maybe ApplicationRevisionSortBy)
larSortBy = lens _larSortBy (\ s a -> s{_larSortBy = a})

-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
larApplicationName :: Lens' ListApplicationRevisions Text
larApplicationName = lens _larApplicationName (\ s a -> s{_larApplicationName = a})

instance AWSPager ListApplicationRevisions where
        page rq rs
          | stop (rs ^. larrsNextToken) = Nothing
          | stop (rs ^. larrsRevisions) = Nothing
          | otherwise =
            Just $ rq & larNextToken .~ rs ^. larrsNextToken

instance AWSRequest ListApplicationRevisions where
        type Rs ListApplicationRevisions =
             ListApplicationRevisionsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 ListApplicationRevisionsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "revisions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListApplicationRevisions where

instance NFData ListApplicationRevisions where

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
              (catMaybes
                 [("s3KeyPrefix" .=) <$> _larS3KeyPrefix,
                  ("deployed" .=) <$> _larDeployed,
                  ("sortOrder" .=) <$> _larSortOrder,
                  ("nextToken" .=) <$> _larNextToken,
                  ("s3Bucket" .=) <$> _larS3Bucket,
                  ("sortBy" .=) <$> _larSortBy,
                  Just ("applicationName" .= _larApplicationName)])

instance ToPath ListApplicationRevisions where
        toPath = const "/"

instance ToQuery ListApplicationRevisions where
        toQuery = const mempty

-- | Represents the output of a ListApplicationRevisions operation.
--
--
--
-- /See:/ 'listApplicationRevisionsResponse' smart constructor.
data ListApplicationRevisionsResponse = ListApplicationRevisionsResponse'
  { _larrsNextToken      :: !(Maybe Text)
  , _larrsRevisions      :: !(Maybe [RevisionLocation])
  , _larrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApplicationRevisionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larrsNextToken' - If a large amount of information is returned, an identifier will also be returned. It can be used in a subsequent list application revisions call to return the next set of application revisions in the list.
--
-- * 'larrsRevisions' - A list of locations that contain the matching revisions.
--
-- * 'larrsResponseStatus' - -- | The response status code.
listApplicationRevisionsResponse
    :: Int -- ^ 'larrsResponseStatus'
    -> ListApplicationRevisionsResponse
listApplicationRevisionsResponse pResponseStatus_ =
  ListApplicationRevisionsResponse'
    { _larrsNextToken = Nothing
    , _larrsRevisions = Nothing
    , _larrsResponseStatus = pResponseStatus_
    }


-- | If a large amount of information is returned, an identifier will also be returned. It can be used in a subsequent list application revisions call to return the next set of application revisions in the list.
larrsNextToken :: Lens' ListApplicationRevisionsResponse (Maybe Text)
larrsNextToken = lens _larrsNextToken (\ s a -> s{_larrsNextToken = a})

-- | A list of locations that contain the matching revisions.
larrsRevisions :: Lens' ListApplicationRevisionsResponse [RevisionLocation]
larrsRevisions = lens _larrsRevisions (\ s a -> s{_larrsRevisions = a}) . _Default . _Coerce

-- | -- | The response status code.
larrsResponseStatus :: Lens' ListApplicationRevisionsResponse Int
larrsResponseStatus = lens _larrsResponseStatus (\ s a -> s{_larrsResponseStatus = a})

instance NFData ListApplicationRevisionsResponse
         where
