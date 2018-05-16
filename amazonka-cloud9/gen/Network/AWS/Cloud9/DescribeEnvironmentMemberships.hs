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
-- Module      : Network.AWS.Cloud9.DescribeEnvironmentMemberships
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about environment members for an AWS Cloud9 development environment.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.DescribeEnvironmentMemberships
    (
    -- * Creating a Request
      describeEnvironmentMemberships
    , DescribeEnvironmentMemberships
    -- * Request Lenses
    , dUserARN
    , dNextToken
    , dPermissions
    , dEnvironmentId
    , dMaxResults

    -- * Destructuring the Response
    , describeEnvironmentMembershipsResponse
    , DescribeEnvironmentMembershipsResponse
    -- * Response Lenses
    , drsNextToken
    , drsMemberships
    , drsResponseStatus
    ) where

import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEnvironmentMemberships' smart constructor.
data DescribeEnvironmentMemberships = DescribeEnvironmentMemberships'
  { _dUserARN       :: !(Maybe Text)
  , _dNextToken     :: !(Maybe Text)
  , _dPermissions   :: !(Maybe [Permissions])
  , _dEnvironmentId :: !(Maybe Text)
  , _dMaxResults    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironmentMemberships' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dUserARN' - The Amazon Resource Name (ARN) of an individual environment member to get information about. If no value is specified, information about all environment members are returned.
--
-- * 'dNextToken' - During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'dPermissions' - The type of environment member permissions to get information about. Available values include:     * @owner@ : Owns the environment.     * @read-only@ : Has read-only access to the environment.     * @read-write@ : Has read-write access to the environment. If no value is specified, information about all environment members are returned.
--
-- * 'dEnvironmentId' - The ID of the environment to get environment member information about.
--
-- * 'dMaxResults' - The maximum number of environment members to get information about.
describeEnvironmentMemberships
    :: DescribeEnvironmentMemberships
describeEnvironmentMemberships =
  DescribeEnvironmentMemberships'
    { _dUserARN = Nothing
    , _dNextToken = Nothing
    , _dPermissions = Nothing
    , _dEnvironmentId = Nothing
    , _dMaxResults = Nothing
    }


-- | The Amazon Resource Name (ARN) of an individual environment member to get information about. If no value is specified, information about all environment members are returned.
dUserARN :: Lens' DescribeEnvironmentMemberships (Maybe Text)
dUserARN = lens _dUserARN (\ s a -> s{_dUserARN = a})

-- | During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
dNextToken :: Lens' DescribeEnvironmentMemberships (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | The type of environment member permissions to get information about. Available values include:     * @owner@ : Owns the environment.     * @read-only@ : Has read-only access to the environment.     * @read-write@ : Has read-write access to the environment. If no value is specified, information about all environment members are returned.
dPermissions :: Lens' DescribeEnvironmentMemberships [Permissions]
dPermissions = lens _dPermissions (\ s a -> s{_dPermissions = a}) . _Default . _Coerce

-- | The ID of the environment to get environment member information about.
dEnvironmentId :: Lens' DescribeEnvironmentMemberships (Maybe Text)
dEnvironmentId = lens _dEnvironmentId (\ s a -> s{_dEnvironmentId = a})

-- | The maximum number of environment members to get information about.
dMaxResults :: Lens' DescribeEnvironmentMemberships (Maybe Natural)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a}) . mapping _Nat

instance AWSPager DescribeEnvironmentMemberships
         where
        page rq rs
          | stop (rs ^. drsNextToken) = Nothing
          | stop (rs ^. drsMemberships) = Nothing
          | otherwise =
            Just $ rq & dNextToken .~ rs ^. drsNextToken

instance AWSRequest DescribeEnvironmentMemberships
         where
        type Rs DescribeEnvironmentMemberships =
             DescribeEnvironmentMembershipsResponse
        request = postJSON cloud9
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEnvironmentMembershipsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "memberships" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEnvironmentMemberships
         where

instance NFData DescribeEnvironmentMemberships where

instance ToHeaders DescribeEnvironmentMemberships
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCloud9WorkspaceManagementService.DescribeEnvironmentMemberships"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEnvironmentMemberships where
        toJSON DescribeEnvironmentMemberships'{..}
          = object
              (catMaybes
                 [("userArn" .=) <$> _dUserARN,
                  ("nextToken" .=) <$> _dNextToken,
                  ("permissions" .=) <$> _dPermissions,
                  ("environmentId" .=) <$> _dEnvironmentId,
                  ("maxResults" .=) <$> _dMaxResults])

instance ToPath DescribeEnvironmentMemberships where
        toPath = const "/"

instance ToQuery DescribeEnvironmentMemberships where
        toQuery = const mempty

-- | /See:/ 'describeEnvironmentMembershipsResponse' smart constructor.
data DescribeEnvironmentMembershipsResponse = DescribeEnvironmentMembershipsResponse'
  { _drsNextToken      :: !(Maybe Text)
  , _drsMemberships    :: !(Maybe [EnvironmentMember])
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironmentMembershipsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsNextToken' - If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- * 'drsMemberships' - Information about the environment members for the environment.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeEnvironmentMembershipsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeEnvironmentMembershipsResponse
describeEnvironmentMembershipsResponse pResponseStatus_ =
  DescribeEnvironmentMembershipsResponse'
    { _drsNextToken = Nothing
    , _drsMemberships = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
drsNextToken :: Lens' DescribeEnvironmentMembershipsResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | Information about the environment members for the environment.
drsMemberships :: Lens' DescribeEnvironmentMembershipsResponse [EnvironmentMember]
drsMemberships = lens _drsMemberships (\ s a -> s{_drsMemberships = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeEnvironmentMembershipsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData
           DescribeEnvironmentMembershipsResponse
         where
