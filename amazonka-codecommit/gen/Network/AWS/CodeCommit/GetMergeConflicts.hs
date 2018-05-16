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
-- Module      : Network.AWS.CodeCommit.GetMergeConflicts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about merge conflicts between the before and after commit IDs for a pull request in a repository.
--
--
module Network.AWS.CodeCommit.GetMergeConflicts
    (
    -- * Creating a Request
      getMergeConflicts
    , GetMergeConflicts
    -- * Request Lenses
    , gmcRepositoryName
    , gmcDestinationCommitSpecifier
    , gmcSourceCommitSpecifier
    , gmcMergeOption

    -- * Destructuring the Response
    , getMergeConflictsResponse
    , GetMergeConflictsResponse
    -- * Response Lenses
    , gmcrsResponseStatus
    , gmcrsMergeable
    , gmcrsDestinationCommitId
    , gmcrsSourceCommitId
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMergeConflicts' smart constructor.
data GetMergeConflicts = GetMergeConflicts'
  { _gmcRepositoryName             :: !Text
  , _gmcDestinationCommitSpecifier :: !Text
  , _gmcSourceCommitSpecifier      :: !Text
  , _gmcMergeOption                :: !MergeOptionTypeEnum
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMergeConflicts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmcRepositoryName' - The name of the repository where the pull request was created.
--
-- * 'gmcDestinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit. For example, a branch name or a full commit ID.
--
-- * 'gmcSourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit. For example, a branch name or a full commit ID.
--
-- * 'gmcMergeOption' - The merge option or strategy you want to use to merge the code. The only valid value is FAST_FORWARD_MERGE.
getMergeConflicts
    :: Text -- ^ 'gmcRepositoryName'
    -> Text -- ^ 'gmcDestinationCommitSpecifier'
    -> Text -- ^ 'gmcSourceCommitSpecifier'
    -> MergeOptionTypeEnum -- ^ 'gmcMergeOption'
    -> GetMergeConflicts
getMergeConflicts pRepositoryName_ pDestinationCommitSpecifier_ pSourceCommitSpecifier_ pMergeOption_ =
  GetMergeConflicts'
    { _gmcRepositoryName = pRepositoryName_
    , _gmcDestinationCommitSpecifier = pDestinationCommitSpecifier_
    , _gmcSourceCommitSpecifier = pSourceCommitSpecifier_
    , _gmcMergeOption = pMergeOption_
    }


-- | The name of the repository where the pull request was created.
gmcRepositoryName :: Lens' GetMergeConflicts Text
gmcRepositoryName = lens _gmcRepositoryName (\ s a -> s{_gmcRepositoryName = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit. For example, a branch name or a full commit ID.
gmcDestinationCommitSpecifier :: Lens' GetMergeConflicts Text
gmcDestinationCommitSpecifier = lens _gmcDestinationCommitSpecifier (\ s a -> s{_gmcDestinationCommitSpecifier = a})

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit. For example, a branch name or a full commit ID.
gmcSourceCommitSpecifier :: Lens' GetMergeConflicts Text
gmcSourceCommitSpecifier = lens _gmcSourceCommitSpecifier (\ s a -> s{_gmcSourceCommitSpecifier = a})

-- | The merge option or strategy you want to use to merge the code. The only valid value is FAST_FORWARD_MERGE.
gmcMergeOption :: Lens' GetMergeConflicts MergeOptionTypeEnum
gmcMergeOption = lens _gmcMergeOption (\ s a -> s{_gmcMergeOption = a})

instance AWSRequest GetMergeConflicts where
        type Rs GetMergeConflicts = GetMergeConflictsResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetMergeConflictsResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "mergeable") <*>
                     (x .:> "destinationCommitId")
                     <*> (x .:> "sourceCommitId"))

instance Hashable GetMergeConflicts where

instance NFData GetMergeConflicts where

instance ToHeaders GetMergeConflicts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetMergeConflicts" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMergeConflicts where
        toJSON GetMergeConflicts'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _gmcRepositoryName),
                  Just
                    ("destinationCommitSpecifier" .=
                       _gmcDestinationCommitSpecifier),
                  Just
                    ("sourceCommitSpecifier" .=
                       _gmcSourceCommitSpecifier),
                  Just ("mergeOption" .= _gmcMergeOption)])

instance ToPath GetMergeConflicts where
        toPath = const "/"

instance ToQuery GetMergeConflicts where
        toQuery = const mempty

-- | /See:/ 'getMergeConflictsResponse' smart constructor.
data GetMergeConflictsResponse = GetMergeConflictsResponse'
  { _gmcrsResponseStatus      :: !Int
  , _gmcrsMergeable           :: !Bool
  , _gmcrsDestinationCommitId :: !Text
  , _gmcrsSourceCommitId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMergeConflictsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmcrsResponseStatus' - -- | The response status code.
--
-- * 'gmcrsMergeable' - A Boolean value that indicates whether the code is mergable by the specified merge option.
--
-- * 'gmcrsDestinationCommitId' - The commit ID of the destination commit specifier that was used in the merge evaluation.
--
-- * 'gmcrsSourceCommitId' - The commit ID of the source commit specifier that was used in the merge evaluation.
getMergeConflictsResponse
    :: Int -- ^ 'gmcrsResponseStatus'
    -> Bool -- ^ 'gmcrsMergeable'
    -> Text -- ^ 'gmcrsDestinationCommitId'
    -> Text -- ^ 'gmcrsSourceCommitId'
    -> GetMergeConflictsResponse
getMergeConflictsResponse pResponseStatus_ pMergeable_ pDestinationCommitId_ pSourceCommitId_ =
  GetMergeConflictsResponse'
    { _gmcrsResponseStatus = pResponseStatus_
    , _gmcrsMergeable = pMergeable_
    , _gmcrsDestinationCommitId = pDestinationCommitId_
    , _gmcrsSourceCommitId = pSourceCommitId_
    }


-- | -- | The response status code.
gmcrsResponseStatus :: Lens' GetMergeConflictsResponse Int
gmcrsResponseStatus = lens _gmcrsResponseStatus (\ s a -> s{_gmcrsResponseStatus = a})

-- | A Boolean value that indicates whether the code is mergable by the specified merge option.
gmcrsMergeable :: Lens' GetMergeConflictsResponse Bool
gmcrsMergeable = lens _gmcrsMergeable (\ s a -> s{_gmcrsMergeable = a})

-- | The commit ID of the destination commit specifier that was used in the merge evaluation.
gmcrsDestinationCommitId :: Lens' GetMergeConflictsResponse Text
gmcrsDestinationCommitId = lens _gmcrsDestinationCommitId (\ s a -> s{_gmcrsDestinationCommitId = a})

-- | The commit ID of the source commit specifier that was used in the merge evaluation.
gmcrsSourceCommitId :: Lens' GetMergeConflictsResponse Text
gmcrsSourceCommitId = lens _gmcrsSourceCommitId (\ s a -> s{_gmcrsSourceCommitId = a})

instance NFData GetMergeConflictsResponse where
