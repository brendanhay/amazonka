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
-- Module      : Network.AWS.IoT.DescribeBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a billing group.
module Network.AWS.IoT.DescribeBillingGroup
  ( -- * Creating a Request
    describeBillingGroup,
    DescribeBillingGroup,

    -- * Request Lenses
    dBillingGroupName,

    -- * Destructuring the Response
    describeBillingGroupResponse,
    DescribeBillingGroupResponse,

    -- * Response Lenses
    dbgbrsBillingGroupARN,
    dbgbrsVersion,
    dbgbrsBillingGroupProperties,
    dbgbrsBillingGroupName,
    dbgbrsBillingGroupId,
    dbgbrsBillingGroupMetadata,
    dbgbrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBillingGroup' smart constructor.
newtype DescribeBillingGroup = DescribeBillingGroup'
  { _dBillingGroupName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBillingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dBillingGroupName' - The name of the billing group.
describeBillingGroup ::
  -- | 'dBillingGroupName'
  Text ->
  DescribeBillingGroup
describeBillingGroup pBillingGroupName_ =
  DescribeBillingGroup' {_dBillingGroupName = pBillingGroupName_}

-- | The name of the billing group.
dBillingGroupName :: Lens' DescribeBillingGroup Text
dBillingGroupName = lens _dBillingGroupName (\s a -> s {_dBillingGroupName = a})

instance AWSRequest DescribeBillingGroup where
  type Rs DescribeBillingGroup = DescribeBillingGroupResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeBillingGroupResponse'
            <$> (x .?> "billingGroupArn")
            <*> (x .?> "version")
            <*> (x .?> "billingGroupProperties")
            <*> (x .?> "billingGroupName")
            <*> (x .?> "billingGroupId")
            <*> (x .?> "billingGroupMetadata")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeBillingGroup

instance NFData DescribeBillingGroup

instance ToHeaders DescribeBillingGroup where
  toHeaders = const mempty

instance ToPath DescribeBillingGroup where
  toPath DescribeBillingGroup' {..} =
    mconcat ["/billing-groups/", toBS _dBillingGroupName]

instance ToQuery DescribeBillingGroup where
  toQuery = const mempty

-- | /See:/ 'describeBillingGroupResponse' smart constructor.
data DescribeBillingGroupResponse = DescribeBillingGroupResponse'
  { _dbgbrsBillingGroupARN ::
      !(Maybe Text),
    _dbgbrsVersion ::
      !(Maybe Integer),
    _dbgbrsBillingGroupProperties ::
      !(Maybe BillingGroupProperties),
    _dbgbrsBillingGroupName ::
      !(Maybe Text),
    _dbgbrsBillingGroupId ::
      !(Maybe Text),
    _dbgbrsBillingGroupMetadata ::
      !(Maybe BillingGroupMetadata),
    _dbgbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBillingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbgbrsBillingGroupARN' - The ARN of the billing group.
--
-- * 'dbgbrsVersion' - The version of the billing group.
--
-- * 'dbgbrsBillingGroupProperties' - The properties of the billing group.
--
-- * 'dbgbrsBillingGroupName' - The name of the billing group.
--
-- * 'dbgbrsBillingGroupId' - The ID of the billing group.
--
-- * 'dbgbrsBillingGroupMetadata' - Additional information about the billing group.
--
-- * 'dbgbrsResponseStatus' - -- | The response status code.
describeBillingGroupResponse ::
  -- | 'dbgbrsResponseStatus'
  Int ->
  DescribeBillingGroupResponse
describeBillingGroupResponse pResponseStatus_ =
  DescribeBillingGroupResponse'
    { _dbgbrsBillingGroupARN = Nothing,
      _dbgbrsVersion = Nothing,
      _dbgbrsBillingGroupProperties = Nothing,
      _dbgbrsBillingGroupName = Nothing,
      _dbgbrsBillingGroupId = Nothing,
      _dbgbrsBillingGroupMetadata = Nothing,
      _dbgbrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the billing group.
dbgbrsBillingGroupARN :: Lens' DescribeBillingGroupResponse (Maybe Text)
dbgbrsBillingGroupARN = lens _dbgbrsBillingGroupARN (\s a -> s {_dbgbrsBillingGroupARN = a})

-- | The version of the billing group.
dbgbrsVersion :: Lens' DescribeBillingGroupResponse (Maybe Integer)
dbgbrsVersion = lens _dbgbrsVersion (\s a -> s {_dbgbrsVersion = a})

-- | The properties of the billing group.
dbgbrsBillingGroupProperties :: Lens' DescribeBillingGroupResponse (Maybe BillingGroupProperties)
dbgbrsBillingGroupProperties = lens _dbgbrsBillingGroupProperties (\s a -> s {_dbgbrsBillingGroupProperties = a})

-- | The name of the billing group.
dbgbrsBillingGroupName :: Lens' DescribeBillingGroupResponse (Maybe Text)
dbgbrsBillingGroupName = lens _dbgbrsBillingGroupName (\s a -> s {_dbgbrsBillingGroupName = a})

-- | The ID of the billing group.
dbgbrsBillingGroupId :: Lens' DescribeBillingGroupResponse (Maybe Text)
dbgbrsBillingGroupId = lens _dbgbrsBillingGroupId (\s a -> s {_dbgbrsBillingGroupId = a})

-- | Additional information about the billing group.
dbgbrsBillingGroupMetadata :: Lens' DescribeBillingGroupResponse (Maybe BillingGroupMetadata)
dbgbrsBillingGroupMetadata = lens _dbgbrsBillingGroupMetadata (\s a -> s {_dbgbrsBillingGroupMetadata = a})

-- | -- | The response status code.
dbgbrsResponseStatus :: Lens' DescribeBillingGroupResponse Int
dbgbrsResponseStatus = lens _dbgbrsResponseStatus (\s a -> s {_dbgbrsResponseStatus = a})

instance NFData DescribeBillingGroupResponse
