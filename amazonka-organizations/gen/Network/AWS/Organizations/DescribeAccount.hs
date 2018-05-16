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
-- Module      : Network.AWS.Organizations.DescribeAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves Organizations-related information about the specified account.
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.DescribeAccount
    (
    -- * Creating a Request
      describeAccount
    , DescribeAccount
    -- * Request Lenses
    , daAccountId

    -- * Destructuring the Response
    , describeAccountResponse
    , DescribeAccountResponse
    -- * Response Lenses
    , darsAccount
    , darsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAccount' smart constructor.
newtype DescribeAccount = DescribeAccount'
  { _daAccountId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAccountId' - The unique identifier (ID) of the AWS account that you want information about. You can get the ID from the 'ListAccounts' or 'ListAccountsForParent' operations. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
describeAccount
    :: Text -- ^ 'daAccountId'
    -> DescribeAccount
describeAccount pAccountId_ = DescribeAccount' {_daAccountId = pAccountId_}


-- | The unique identifier (ID) of the AWS account that you want information about. You can get the ID from the 'ListAccounts' or 'ListAccountsForParent' operations. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
daAccountId :: Lens' DescribeAccount Text
daAccountId = lens _daAccountId (\ s a -> s{_daAccountId = a})

instance AWSRequest DescribeAccount where
        type Rs DescribeAccount = DescribeAccountResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAccountResponse' <$>
                   (x .?> "Account") <*> (pure (fromEnum s)))

instance Hashable DescribeAccount where

instance NFData DescribeAccount where

instance ToHeaders DescribeAccount where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DescribeAccount" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAccount where
        toJSON DescribeAccount'{..}
          = object
              (catMaybes [Just ("AccountId" .= _daAccountId)])

instance ToPath DescribeAccount where
        toPath = const "/"

instance ToQuery DescribeAccount where
        toQuery = const mempty

-- | /See:/ 'describeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { _darsAccount        :: !(Maybe Account)
  , _darsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAccount' - A structure that contains information about the requested account.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAccountResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAccountResponse
describeAccountResponse pResponseStatus_ =
  DescribeAccountResponse'
    {_darsAccount = Nothing, _darsResponseStatus = pResponseStatus_}


-- | A structure that contains information about the requested account.
darsAccount :: Lens' DescribeAccountResponse (Maybe Account)
darsAccount = lens _darsAccount (\ s a -> s{_darsAccount = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAccountResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAccountResponse where
