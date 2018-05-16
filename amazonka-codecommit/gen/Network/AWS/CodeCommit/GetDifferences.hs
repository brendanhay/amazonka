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
-- Module      : Network.AWS.CodeCommit.GetDifferences
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference). Results can be limited to a specified path.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetDifferences
    (
    -- * Creating a Request
      getDifferences
    , GetDifferences
    -- * Request Lenses
    , gdAfterPath
    , gdNextToken
    , gdBeforeCommitSpecifier
    , gdBeforePath
    , gdMaxResults
    , gdRepositoryName
    , gdAfterCommitSpecifier

    -- * Destructuring the Response
    , getDifferencesResponse
    , GetDifferencesResponse
    -- * Response Lenses
    , gdrsNextToken
    , gdrsDifferences
    , gdrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDifferences' smart constructor.
data GetDifferences = GetDifferences'
  { _gdAfterPath             :: !(Maybe Text)
  , _gdNextToken             :: !(Maybe Text)
  , _gdBeforeCommitSpecifier :: !(Maybe Text)
  , _gdBeforePath            :: !(Maybe Text)
  , _gdMaxResults            :: !(Maybe Int)
  , _gdRepositoryName        :: !Text
  , _gdAfterCommitSpecifier  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDifferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdAfterPath' - The file path in which to check differences. Limits the results to this path. Can also be used to specify the changed name of a directory or folder, if it has changed. If not specified, differences will be shown for all paths.
--
-- * 'gdNextToken' - An enumeration token that when provided in a request, returns the next batch of the results.
--
-- * 'gdBeforeCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit. For example, the full commit ID. Optional. If not specified, all changes prior to the @afterCommitSpecifier@ value will be shown. If you do not use @beforeCommitSpecifier@ in your request, consider limiting the results with @maxResults@ .
--
-- * 'gdBeforePath' - The file path in which to check for differences. Limits the results to this path. Can also be used to specify the previous name of a directory or folder. If @beforePath@ and @afterPath@ are not specified, differences will be shown for all paths.
--
-- * 'gdMaxResults' - A non-negative integer used to limit the number of returned results.
--
-- * 'gdRepositoryName' - The name of the repository where you want to get differences.
--
-- * 'gdAfterCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit.
getDifferences
    :: Text -- ^ 'gdRepositoryName'
    -> Text -- ^ 'gdAfterCommitSpecifier'
    -> GetDifferences
getDifferences pRepositoryName_ pAfterCommitSpecifier_ =
  GetDifferences'
    { _gdAfterPath = Nothing
    , _gdNextToken = Nothing
    , _gdBeforeCommitSpecifier = Nothing
    , _gdBeforePath = Nothing
    , _gdMaxResults = Nothing
    , _gdRepositoryName = pRepositoryName_
    , _gdAfterCommitSpecifier = pAfterCommitSpecifier_
    }


-- | The file path in which to check differences. Limits the results to this path. Can also be used to specify the changed name of a directory or folder, if it has changed. If not specified, differences will be shown for all paths.
gdAfterPath :: Lens' GetDifferences (Maybe Text)
gdAfterPath = lens _gdAfterPath (\ s a -> s{_gdAfterPath = a})

-- | An enumeration token that when provided in a request, returns the next batch of the results.
gdNextToken :: Lens' GetDifferences (Maybe Text)
gdNextToken = lens _gdNextToken (\ s a -> s{_gdNextToken = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit. For example, the full commit ID. Optional. If not specified, all changes prior to the @afterCommitSpecifier@ value will be shown. If you do not use @beforeCommitSpecifier@ in your request, consider limiting the results with @maxResults@ .
gdBeforeCommitSpecifier :: Lens' GetDifferences (Maybe Text)
gdBeforeCommitSpecifier = lens _gdBeforeCommitSpecifier (\ s a -> s{_gdBeforeCommitSpecifier = a})

-- | The file path in which to check for differences. Limits the results to this path. Can also be used to specify the previous name of a directory or folder. If @beforePath@ and @afterPath@ are not specified, differences will be shown for all paths.
gdBeforePath :: Lens' GetDifferences (Maybe Text)
gdBeforePath = lens _gdBeforePath (\ s a -> s{_gdBeforePath = a})

-- | A non-negative integer used to limit the number of returned results.
gdMaxResults :: Lens' GetDifferences (Maybe Int)
gdMaxResults = lens _gdMaxResults (\ s a -> s{_gdMaxResults = a})

-- | The name of the repository where you want to get differences.
gdRepositoryName :: Lens' GetDifferences Text
gdRepositoryName = lens _gdRepositoryName (\ s a -> s{_gdRepositoryName = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit.
gdAfterCommitSpecifier :: Lens' GetDifferences Text
gdAfterCommitSpecifier = lens _gdAfterCommitSpecifier (\ s a -> s{_gdAfterCommitSpecifier = a})

instance AWSPager GetDifferences where
        page rq rs
          | stop (rs ^. gdrsNextToken) = Nothing
          | stop (rs ^. gdrsDifferences) = Nothing
          | otherwise =
            Just $ rq & gdNextToken .~ rs ^. gdrsNextToken

instance AWSRequest GetDifferences where
        type Rs GetDifferences = GetDifferencesResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetDifferencesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "differences" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetDifferences where

instance NFData GetDifferences where

instance ToHeaders GetDifferences where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetDifferences" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDifferences where
        toJSON GetDifferences'{..}
          = object
              (catMaybes
                 [("afterPath" .=) <$> _gdAfterPath,
                  ("NextToken" .=) <$> _gdNextToken,
                  ("beforeCommitSpecifier" .=) <$>
                    _gdBeforeCommitSpecifier,
                  ("beforePath" .=) <$> _gdBeforePath,
                  ("MaxResults" .=) <$> _gdMaxResults,
                  Just ("repositoryName" .= _gdRepositoryName),
                  Just
                    ("afterCommitSpecifier" .= _gdAfterCommitSpecifier)])

instance ToPath GetDifferences where
        toPath = const "/"

instance ToQuery GetDifferences where
        toQuery = const mempty

-- | /See:/ 'getDifferencesResponse' smart constructor.
data GetDifferencesResponse = GetDifferencesResponse'
  { _gdrsNextToken      :: !(Maybe Text)
  , _gdrsDifferences    :: !(Maybe [Difference])
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDifferencesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsNextToken' - An enumeration token that can be used in a request to return the next batch of the results.
--
-- * 'gdrsDifferences' - A differences data type object that contains information about the differences, including whether the difference is added, modified, or deleted (A, D, M).
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDifferencesResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDifferencesResponse
getDifferencesResponse pResponseStatus_ =
  GetDifferencesResponse'
    { _gdrsNextToken = Nothing
    , _gdrsDifferences = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }


-- | An enumeration token that can be used in a request to return the next batch of the results.
gdrsNextToken :: Lens' GetDifferencesResponse (Maybe Text)
gdrsNextToken = lens _gdrsNextToken (\ s a -> s{_gdrsNextToken = a})

-- | A differences data type object that contains information about the differences, including whether the difference is added, modified, or deleted (A, D, M).
gdrsDifferences :: Lens' GetDifferencesResponse [Difference]
gdrsDifferences = lens _gdrsDifferences (\ s a -> s{_gdrsDifferences = a}) . _Default . _Coerce

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDifferencesResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDifferencesResponse where
