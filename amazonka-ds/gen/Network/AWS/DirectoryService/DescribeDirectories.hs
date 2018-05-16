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
-- Module      : Network.AWS.DirectoryService.DescribeDirectories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directories that belong to this account.
--
--
-- You can retrieve information about specific directories by passing the directory identifiers in the /DirectoryIds/ parameter. Otherwise, all directories that belong to the current account are returned.
--
-- This operation supports pagination with the use of the /NextToken/ request and response parameters. If more results are available, the /DescribeDirectoriesResult.NextToken/ member contains a token that you pass in the next call to 'DescribeDirectories' to retrieve the next set of items.
--
-- You can also specify a maximum number of return results with the /Limit/ parameter.
--
module Network.AWS.DirectoryService.DescribeDirectories
    (
    -- * Creating a Request
      describeDirectories
    , DescribeDirectories
    -- * Request Lenses
    , ddNextToken
    , ddDirectoryIds
    , ddLimit

    -- * Destructuring the Response
    , describeDirectoriesResponse
    , DescribeDirectoriesResponse
    -- * Response Lenses
    , ddrsDirectoryDescriptions
    , ddrsNextToken
    , ddrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'DescribeDirectories' operation.
--
--
--
-- /See:/ 'describeDirectories' smart constructor.
data DescribeDirectories = DescribeDirectories'
  { _ddNextToken    :: !(Maybe Text)
  , _ddDirectoryIds :: !(Maybe [Text])
  , _ddLimit        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddNextToken' - The /DescribeDirectoriesResult.NextToken/ value from a previous call to 'DescribeDirectories' . Pass null if this is the first call.
--
-- * 'ddDirectoryIds' - A list of identifiers of the directories for which to obtain the information. If this member is null, all directories that belong to the current account are returned. An empty list results in an @InvalidParameterException@ being thrown.
--
-- * 'ddLimit' - The maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
describeDirectories
    :: DescribeDirectories
describeDirectories =
  DescribeDirectories'
    {_ddNextToken = Nothing, _ddDirectoryIds = Nothing, _ddLimit = Nothing}


-- | The /DescribeDirectoriesResult.NextToken/ value from a previous call to 'DescribeDirectories' . Pass null if this is the first call.
ddNextToken :: Lens' DescribeDirectories (Maybe Text)
ddNextToken = lens _ddNextToken (\ s a -> s{_ddNextToken = a})

-- | A list of identifiers of the directories for which to obtain the information. If this member is null, all directories that belong to the current account are returned. An empty list results in an @InvalidParameterException@ being thrown.
ddDirectoryIds :: Lens' DescribeDirectories [Text]
ddDirectoryIds = lens _ddDirectoryIds (\ s a -> s{_ddDirectoryIds = a}) . _Default . _Coerce

-- | The maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
ddLimit :: Lens' DescribeDirectories (Maybe Natural)
ddLimit = lens _ddLimit (\ s a -> s{_ddLimit = a}) . mapping _Nat

instance AWSRequest DescribeDirectories where
        type Rs DescribeDirectories =
             DescribeDirectoriesResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDirectoriesResponse' <$>
                   (x .?> "DirectoryDescriptions" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDirectories where

instance NFData DescribeDirectories where

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
              (catMaybes
                 [("NextToken" .=) <$> _ddNextToken,
                  ("DirectoryIds" .=) <$> _ddDirectoryIds,
                  ("Limit" .=) <$> _ddLimit])

instance ToPath DescribeDirectories where
        toPath = const "/"

instance ToQuery DescribeDirectories where
        toQuery = const mempty

-- | Contains the results of the 'DescribeDirectories' operation.
--
--
--
-- /See:/ 'describeDirectoriesResponse' smart constructor.
data DescribeDirectoriesResponse = DescribeDirectoriesResponse'
  { _ddrsDirectoryDescriptions :: !(Maybe [DirectoryDescription])
  , _ddrsNextToken             :: !(Maybe Text)
  , _ddrsResponseStatus        :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsDirectoryDescriptions' - The list of 'DirectoryDescription' objects that were retrieved. It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
--
-- * 'ddrsNextToken' - If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeDirectories' to retrieve the next set of items.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
describeDirectoriesResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DescribeDirectoriesResponse
describeDirectoriesResponse pResponseStatus_ =
  DescribeDirectoriesResponse'
    { _ddrsDirectoryDescriptions = Nothing
    , _ddrsNextToken = Nothing
    , _ddrsResponseStatus = pResponseStatus_
    }


-- | The list of 'DirectoryDescription' objects that were retrieved. It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
ddrsDirectoryDescriptions :: Lens' DescribeDirectoriesResponse [DirectoryDescription]
ddrsDirectoryDescriptions = lens _ddrsDirectoryDescriptions (\ s a -> s{_ddrsDirectoryDescriptions = a}) . _Default . _Coerce

-- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeDirectories' to retrieve the next set of items.
ddrsNextToken :: Lens' DescribeDirectoriesResponse (Maybe Text)
ddrsNextToken = lens _ddrsNextToken (\ s a -> s{_ddrsNextToken = a})

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeDirectoriesResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DescribeDirectoriesResponse where
