{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectoryService.DescribeDirectories
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

-- | Obtains information about the directories that belong to this account.
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
    , ddNextToken
    , ddDirectoryIds
    , ddLimit

    -- * Response
    , DescribeDirectoriesResponse
    -- ** Response constructor
    , describeDirectoriesResponse
    -- ** Response lenses
    , ddrDirectoryDescriptions
    , ddrNextToken
    , ddrStatus
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
-- * 'ddNextToken'
--
-- * 'ddDirectoryIds'
--
-- * 'ddLimit'
data DescribeDirectories = DescribeDirectories'
    { _ddNextToken    :: !(Maybe Text)
    , _ddDirectoryIds :: !(Maybe [Text])
    , _ddLimit        :: !(Maybe Nat)
    } deriving (Eq,Read,Show)

-- | 'DescribeDirectories' smart constructor.
describeDirectories :: DescribeDirectories
describeDirectories =
    DescribeDirectories'
    { _ddNextToken = Nothing
    , _ddDirectoryIds = Nothing
    , _ddLimit = Nothing
    }

-- | The /DescribeDirectoriesResult.NextToken/ value from a previous call to
-- DescribeDirectories. Pass null if this is the first call.
ddNextToken :: Lens' DescribeDirectories (Maybe Text)
ddNextToken = lens _ddNextToken (\ s a -> s{_ddNextToken = a});

-- | A list of identifiers of the directories to obtain the information for.
-- If this member is null, all directories that belong to the current
-- account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
ddDirectoryIds :: Lens' DescribeDirectories [Text]
ddDirectoryIds = lens _ddDirectoryIds (\ s a -> s{_ddDirectoryIds = a}) . _Default;

-- | The maximum number of items to return. If this value is zero, the
-- maximum number of items is specified by the limitations of the
-- operation.
ddLimit :: Lens' DescribeDirectories (Maybe Natural)
ddLimit = lens _ddLimit (\ s a -> s{_ddLimit = a}) . mapping _Nat;

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
                     <*> (pure s))

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
              ["NextToken" .= _ddNextToken,
               "DirectoryIds" .= _ddDirectoryIds,
               "Limit" .= _ddLimit]

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
-- * 'ddrDirectoryDescriptions'
--
-- * 'ddrNextToken'
--
-- * 'ddrStatus'
data DescribeDirectoriesResponse = DescribeDirectoriesResponse'
    { _ddrDirectoryDescriptions :: !(Maybe [DirectoryDescription])
    , _ddrNextToken             :: !(Maybe Text)
    , _ddrStatus                :: !Status
    } deriving (Eq,Show)

-- | 'DescribeDirectoriesResponse' smart constructor.
describeDirectoriesResponse :: Status -> DescribeDirectoriesResponse
describeDirectoriesResponse pStatus =
    DescribeDirectoriesResponse'
    { _ddrDirectoryDescriptions = Nothing
    , _ddrNextToken = Nothing
    , _ddrStatus = pStatus
    }

-- | The list of DirectoryDescription objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
ddrDirectoryDescriptions :: Lens' DescribeDirectoriesResponse [DirectoryDescription]
ddrDirectoryDescriptions = lens _ddrDirectoryDescriptions (\ s a -> s{_ddrDirectoryDescriptions = a}) . _Default;

-- | If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to DescribeDirectories to
-- retrieve the next set of items.
ddrNextToken :: Lens' DescribeDirectoriesResponse (Maybe Text)
ddrNextToken = lens _ddrNextToken (\ s a -> s{_ddrNextToken = a});

-- | FIXME: Undocumented member.
ddrStatus :: Lens' DescribeDirectoriesResponse Status
ddrStatus = lens _ddrStatus (\ s a -> s{_ddrStatus = a});
