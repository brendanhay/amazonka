{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeCreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of an asynchronous request to create an account.
--
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeCreateAccountStatus
  ( -- * Creating a Request
    describeCreateAccountStatus,
    DescribeCreateAccountStatus,

    -- * Request Lenses
    dcasCreateAccountRequestId,

    -- * Destructuring the Response
    describeCreateAccountStatusResponse,
    DescribeCreateAccountStatusResponse,

    -- * Response Lenses
    dcasrsCreateAccountStatus,
    dcasrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCreateAccountStatus' smart constructor.
newtype DescribeCreateAccountStatus = DescribeCreateAccountStatus'
  { _dcasCreateAccountRequestId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCreateAccountStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcasCreateAccountRequestId' - Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
describeCreateAccountStatus ::
  -- | 'dcasCreateAccountRequestId'
  Text ->
  DescribeCreateAccountStatus
describeCreateAccountStatus pCreateAccountRequestId_ =
  DescribeCreateAccountStatus'
    { _dcasCreateAccountRequestId =
        pCreateAccountRequestId_
    }

-- | Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
dcasCreateAccountRequestId :: Lens' DescribeCreateAccountStatus Text
dcasCreateAccountRequestId = lens _dcasCreateAccountRequestId (\s a -> s {_dcasCreateAccountRequestId = a})

instance AWSRequest DescribeCreateAccountStatus where
  type
    Rs DescribeCreateAccountStatus =
      DescribeCreateAccountStatusResponse
  request = postJSON organizations
  response =
    receiveJSON
      ( \s h x ->
          DescribeCreateAccountStatusResponse'
            <$> (x .?> "CreateAccountStatus") <*> (pure (fromEnum s))
      )

instance Hashable DescribeCreateAccountStatus

instance NFData DescribeCreateAccountStatus

instance ToHeaders DescribeCreateAccountStatus where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSOrganizationsV20161128.DescribeCreateAccountStatus" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeCreateAccountStatus where
  toJSON DescribeCreateAccountStatus' {..} =
    object
      ( catMaybes
          [Just ("CreateAccountRequestId" .= _dcasCreateAccountRequestId)]
      )

instance ToPath DescribeCreateAccountStatus where
  toPath = const "/"

instance ToQuery DescribeCreateAccountStatus where
  toQuery = const mempty

-- | /See:/ 'describeCreateAccountStatusResponse' smart constructor.
data DescribeCreateAccountStatusResponse = DescribeCreateAccountStatusResponse'
  { _dcasrsCreateAccountStatus ::
      !( Maybe
           CreateAccountStatus
       ),
    _dcasrsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCreateAccountStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcasrsCreateAccountStatus' - A structure that contains the current status of an account creation request.
--
-- * 'dcasrsResponseStatus' - -- | The response status code.
describeCreateAccountStatusResponse ::
  -- | 'dcasrsResponseStatus'
  Int ->
  DescribeCreateAccountStatusResponse
describeCreateAccountStatusResponse pResponseStatus_ =
  DescribeCreateAccountStatusResponse'
    { _dcasrsCreateAccountStatus =
        Nothing,
      _dcasrsResponseStatus = pResponseStatus_
    }

-- | A structure that contains the current status of an account creation request.
dcasrsCreateAccountStatus :: Lens' DescribeCreateAccountStatusResponse (Maybe CreateAccountStatus)
dcasrsCreateAccountStatus = lens _dcasrsCreateAccountStatus (\s a -> s {_dcasrsCreateAccountStatus = a})

-- | -- | The response status code.
dcasrsResponseStatus :: Lens' DescribeCreateAccountStatusResponse Int
dcasrsResponseStatus = lens _dcasrsResponseStatus (\s a -> s {_dcasrsResponseStatus = a})

instance NFData DescribeCreateAccountStatusResponse
