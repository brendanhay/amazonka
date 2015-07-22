{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeDirectories
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directories that belong to this account.
--
-- You can retrieve information about specific directories by passing the
-- directory identifiers in the /DirectoryIds/ parameter. Otherwise, all
-- directories that belong to the current account are returned.
--
-- This operation supports pagination with the use of the /NextToken/
-- request and response parameters. If more results are available, the
-- /DescribeDirectoriesResult.NextToken/ member contains a token that you
-- pass in the next call to DescribeDirectories to retrieve the next set of
-- items.
--
-- You can also specify a maximum number of return results with the /Limit/
-- parameter.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DescribeDirectories.html>
module Network.AWS.DirectoryService.DescribeDirectories
    (
    -- * Request
      DescribeDirectories
    -- ** Request constructor
    , describeDirectories
    -- ** Request lenses
    , ddrqNextToken
    , ddrqDirectoryIds
    , ddrqLimit

    -- * Response
    , DescribeDirectoriesResponse
    -- ** Response constructor
    , describeDirectoriesResponse
    -- ** Response lenses
    , ddrsDirectoryDescriptions
    , ddrsNextToken
    , ddrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DescribeDirectories operation.
--
-- /See:/ 'describeDirectories' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrqNextToken'
--
-- * 'ddrqDirectoryIds'
--
-- * 'ddrqLimit'
data DescribeDirectories = DescribeDirectories'
    { _ddrqNextToken    :: !(Maybe Text)
    , _ddrqDirectoryIds :: !(Maybe [Text])
    , _ddrqLimit        :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDirectories' smart constructor.
describeDirectories :: DescribeDirectories
describeDirectories =
    DescribeDirectories'
    { _ddrqNextToken = Nothing
    , _ddrqDirectoryIds = Nothing
    , _ddrqLimit = Nothing
    }

-- | The /DescribeDirectoriesResult.NextToken/ value from a previous call to
-- DescribeDirectories. Pass null if this is the first call.
ddrqNextToken :: Lens' DescribeDirectories (Maybe Text)
ddrqNextToken = lens _ddrqNextToken (\ s a -> s{_ddrqNextToken = a});

-- | A list of identifiers of the directories to obtain the information for.
-- If this member is null, all directories that belong to the current
-- account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
ddrqDirectoryIds :: Lens' DescribeDirectories [Text]
ddrqDirectoryIds = lens _ddrqDirectoryIds (\ s a -> s{_ddrqDirectoryIds = a}) . _Default;

-- | The maximum number of items to return. If this value is zero, the
-- maximum number of items is specified by the limitations of the
-- operation.
ddrqLimit :: Lens' DescribeDirectories (Maybe Natural)
ddrqLimit = lens _ddrqLimit (\ s a -> s{_ddrqLimit = a}) . mapping _Nat;

instance AWSRequest DescribeDirectories where
        type Sv DescribeDirectories = DirectoryService
        type Rs DescribeDirectories =
             DescribeDirectoriesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDirectoriesResponse' <$>
                   (x .?> "DirectoryDescriptions" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDirectories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DescribeDirectories" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDirectories where
        toJSON DescribeDirectories'{..}
          = object
              ["NextToken" .= _ddrqNextToken,
               "DirectoryIds" .= _ddrqDirectoryIds,
               "Limit" .= _ddrqLimit]

instance ToPath DescribeDirectories where
        toPath = const "/"

instance ToQuery DescribeDirectories where
        toQuery = const mempty

-- | Contains the results of the DescribeDirectories operation.
--
-- /See:/ 'describeDirectoriesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrsDirectoryDescriptions'
--
-- * 'ddrsNextToken'
--
-- * 'ddrsStatus'
data DescribeDirectoriesResponse = DescribeDirectoriesResponse'
    { _ddrsDirectoryDescriptions :: !(Maybe [DirectoryDescription])
    , _ddrsNextToken             :: !(Maybe Text)
    , _ddrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDirectoriesResponse' smart constructor.
describeDirectoriesResponse :: Int -> DescribeDirectoriesResponse
describeDirectoriesResponse pStatus_ =
    DescribeDirectoriesResponse'
    { _ddrsDirectoryDescriptions = Nothing
    , _ddrsNextToken = Nothing
    , _ddrsStatus = pStatus_
    }

-- | The list of DirectoryDescription objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
ddrsDirectoryDescriptions :: Lens' DescribeDirectoriesResponse [DirectoryDescription]
ddrsDirectoryDescriptions = lens _ddrsDirectoryDescriptions (\ s a -> s{_ddrsDirectoryDescriptions = a}) . _Default;

-- | If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to DescribeDirectories to
-- retrieve the next set of items.
ddrsNextToken :: Lens' DescribeDirectoriesResponse (Maybe Text)
ddrsNextToken = lens _ddrsNextToken (\ s a -> s{_ddrsNextToken = a});

-- | FIXME: Undocumented member.
ddrsStatus :: Lens' DescribeDirectoriesResponse Int
ddrsStatus = lens _ddrsStatus (\ s a -> s{_ddrsStatus = a});
