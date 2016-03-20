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
-- Module      : Network.AWS.CodeCommit.ListBranches
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more branches in a repository.
module Network.AWS.CodeCommit.ListBranches
    (
    -- * Creating a Request
      listBranches
    , ListBranches
    -- * Request Lenses
    , lbNextToken
    , lbRepositoryName

    -- * Destructuring the Response
    , listBranchesResponse
    , ListBranchesResponse
    -- * Response Lenses
    , lbrsBranches
    , lbrsNextToken
    , lbrsResponseStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.CodeCommit.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list branches operation.
--
-- /See:/ 'listBranches' smart constructor.
data ListBranches = ListBranches'
    { _lbNextToken      :: !(Maybe Text)
    , _lbRepositoryName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListBranches' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbNextToken'
--
-- * 'lbRepositoryName'
listBranches
    :: Text -- ^ 'lbRepositoryName'
    -> ListBranches
listBranches pRepositoryName_ =
    ListBranches'
    { _lbNextToken = Nothing
    , _lbRepositoryName = pRepositoryName_
    }

-- | An enumeration token that allows the operation to batch the results.
lbNextToken :: Lens' ListBranches (Maybe Text)
lbNextToken = lens _lbNextToken (\ s a -> s{_lbNextToken = a});

-- | The name of the repository that contains the branches.
lbRepositoryName :: Lens' ListBranches Text
lbRepositoryName = lens _lbRepositoryName (\ s a -> s{_lbRepositoryName = a});

instance AWSRequest ListBranches where
        type Rs ListBranches = ListBranchesResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 ListBranchesResponse' <$>
                   (x .?> "branches" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListBranches

instance ToHeaders ListBranches where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.ListBranches" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListBranches where
        toJSON ListBranches'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lbNextToken,
                  Just ("repositoryName" .= _lbRepositoryName)])

instance ToPath ListBranches where
        toPath = const "/"

instance ToQuery ListBranches where
        toQuery = const mempty

-- | Represents the output of a list branches operation.
--
-- /See:/ 'listBranchesResponse' smart constructor.
data ListBranchesResponse = ListBranchesResponse'
    { _lbrsBranches       :: !(Maybe [Text])
    , _lbrsNextToken      :: !(Maybe Text)
    , _lbrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListBranchesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrsBranches'
--
-- * 'lbrsNextToken'
--
-- * 'lbrsResponseStatus'
listBranchesResponse
    :: Int -- ^ 'lbrsResponseStatus'
    -> ListBranchesResponse
listBranchesResponse pResponseStatus_ =
    ListBranchesResponse'
    { _lbrsBranches = Nothing
    , _lbrsNextToken = Nothing
    , _lbrsResponseStatus = pResponseStatus_
    }

-- | The list of branch names.
lbrsBranches :: Lens' ListBranchesResponse [Text]
lbrsBranches = lens _lbrsBranches (\ s a -> s{_lbrsBranches = a}) . _Default . _Coerce;

-- | An enumeration token that returns the batch of the results.
lbrsNextToken :: Lens' ListBranchesResponse (Maybe Text)
lbrsNextToken = lens _lbrsNextToken (\ s a -> s{_lbrsNextToken = a});

-- | The response status code.
lbrsResponseStatus :: Lens' ListBranchesResponse Int
lbrsResponseStatus = lens _lbrsResponseStatus (\ s a -> s{_lbrsResponseStatus = a});
