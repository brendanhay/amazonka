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
-- Module      : Network.AWS.DMS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the AWS DMS attributes for a customer account. These attributes include AWS DMS quotas for the account and a unique account identifier in a particular DMS region. DMS quotas include a list of resource quotas supported by the account, such as the number of replication instances allowed. The description for each resource quota, includes the quota name, current usage toward that quota, and the quota's maximum value. DMS uses the unique account identifier to name each artifact used by DMS in the given region.
--
--
-- This command does not take any parameters.
module Network.AWS.DMS.DescribeAccountAttributes
  ( -- * Creating a Request
    describeAccountAttributes,
    DescribeAccountAttributes,

    -- * Destructuring the Response
    describeAccountAttributesResponse,
    DescribeAccountAttributesResponse,

    -- * Response Lenses
    daarsAccountQuotas,
    daarsUniqueAccountIdentifier,
    daarsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
describeAccountAttributes ::
  DescribeAccountAttributes
describeAccountAttributes = DescribeAccountAttributes'

instance AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          DescribeAccountAttributesResponse'
            <$> (x .?> "AccountQuotas" .!@ mempty)
            <*> (x .?> "UniqueAccountIdentifier")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAccountAttributes

instance NFData DescribeAccountAttributes

instance ToHeaders DescribeAccountAttributes where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonDMSv20160101.DescribeAccountAttributes" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAccountAttributes where
  toJSON = const (Object mempty)

instance ToPath DescribeAccountAttributes where
  toPath = const "/"

instance ToQuery DescribeAccountAttributes where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { _daarsAccountQuotas ::
      !(Maybe [AccountQuota]),
    _daarsUniqueAccountIdentifier ::
      !(Maybe Text),
    _daarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsAccountQuotas' - Account quota information.
--
-- * 'daarsUniqueAccountIdentifier' - A unique AWS DMS identifier for an account in a particular AWS Region. The value of this identifier has the following format: @c99999999999@ . DMS uses this identifier to name artifacts. For example, DMS uses this identifier to name the default Amazon S3 bucket for storing task assessment reports in a given AWS Region. The format of this S3 bucket name is the following: @dms-/AccountNumber/ -/UniqueAccountIdentifier/ .@ Here is an example name for this default S3 bucket: @dms-111122223333-c44445555666@ .
--
-- * 'daarsResponseStatus' - -- | The response status code.
describeAccountAttributesResponse ::
  -- | 'daarsResponseStatus'
  Int ->
  DescribeAccountAttributesResponse
describeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    { _daarsAccountQuotas = Nothing,
      _daarsUniqueAccountIdentifier = Nothing,
      _daarsResponseStatus = pResponseStatus_
    }

-- | Account quota information.
daarsAccountQuotas :: Lens' DescribeAccountAttributesResponse [AccountQuota]
daarsAccountQuotas = lens _daarsAccountQuotas (\s a -> s {_daarsAccountQuotas = a}) . _Default . _Coerce

-- | A unique AWS DMS identifier for an account in a particular AWS Region. The value of this identifier has the following format: @c99999999999@ . DMS uses this identifier to name artifacts. For example, DMS uses this identifier to name the default Amazon S3 bucket for storing task assessment reports in a given AWS Region. The format of this S3 bucket name is the following: @dms-/AccountNumber/ -/UniqueAccountIdentifier/ .@ Here is an example name for this default S3 bucket: @dms-111122223333-c44445555666@ .
daarsUniqueAccountIdentifier :: Lens' DescribeAccountAttributesResponse (Maybe Text)
daarsUniqueAccountIdentifier = lens _daarsUniqueAccountIdentifier (\s a -> s {_daarsUniqueAccountIdentifier = a})

-- | -- | The response status code.
daarsResponseStatus :: Lens' DescribeAccountAttributesResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\s a -> s {_daarsResponseStatus = a})

instance NFData DescribeAccountAttributesResponse
