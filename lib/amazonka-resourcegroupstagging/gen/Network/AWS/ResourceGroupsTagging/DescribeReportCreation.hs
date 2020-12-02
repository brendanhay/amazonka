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
-- Module      : Network.AWS.ResourceGroupsTagging.DescribeReportCreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the @StartReportCreation@ operation.
--
--
-- You can call this operation only from the organization's master account and from the us-east-1 Region.
module Network.AWS.ResourceGroupsTagging.DescribeReportCreation
  ( -- * Creating a Request
    describeReportCreation,
    DescribeReportCreation,

    -- * Destructuring the Response
    describeReportCreationResponse,
    DescribeReportCreationResponse,

    -- * Response Lenses
    drcrsStatus,
    drcrsS3Location,
    drcrsErrorMessage,
    drcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.Response

-- | /See:/ 'describeReportCreation' smart constructor.
data DescribeReportCreation = DescribeReportCreation'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReportCreation' with the minimum fields required to make a request.
describeReportCreation ::
  DescribeReportCreation
describeReportCreation = DescribeReportCreation'

instance AWSRequest DescribeReportCreation where
  type Rs DescribeReportCreation = DescribeReportCreationResponse
  request = postJSON resourceGroupsTagging
  response =
    receiveJSON
      ( \s h x ->
          DescribeReportCreationResponse'
            <$> (x .?> "Status")
            <*> (x .?> "S3Location")
            <*> (x .?> "ErrorMessage")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReportCreation

instance NFData DescribeReportCreation

instance ToHeaders DescribeReportCreation where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "ResourceGroupsTaggingAPI_20170126.DescribeReportCreation" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeReportCreation where
  toJSON = const (Object mempty)

instance ToPath DescribeReportCreation where
  toPath = const "/"

instance ToQuery DescribeReportCreation where
  toQuery = const mempty

-- | /See:/ 'describeReportCreationResponse' smart constructor.
data DescribeReportCreationResponse = DescribeReportCreationResponse'
  { _drcrsStatus ::
      !(Maybe Text),
    _drcrsS3Location ::
      !(Maybe Text),
    _drcrsErrorMessage ::
      !(Maybe Text),
    _drcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReportCreationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcrsStatus' - Reports the status of the operation. The operation status can be one of the following:     * @RUNNING@ - Report creation is in progress.     * @SUCCEEDED@ - Report creation is complete. You can open the report from the Amazon S3 bucket that you specified when you ran @StartReportCreation@ .     * @FAILED@ - Report creation timed out or the Amazon S3 bucket is not accessible.      * @NO REPORT@ - No report was generated in the last 90 days.
--
-- * 'drcrsS3Location' - The path to the Amazon S3 bucket where the report was stored on creation.
--
-- * 'drcrsErrorMessage' - Details of the common errors that all operations return.
--
-- * 'drcrsResponseStatus' - -- | The response status code.
describeReportCreationResponse ::
  -- | 'drcrsResponseStatus'
  Int ->
  DescribeReportCreationResponse
describeReportCreationResponse pResponseStatus_ =
  DescribeReportCreationResponse'
    { _drcrsStatus = Nothing,
      _drcrsS3Location = Nothing,
      _drcrsErrorMessage = Nothing,
      _drcrsResponseStatus = pResponseStatus_
    }

-- | Reports the status of the operation. The operation status can be one of the following:     * @RUNNING@ - Report creation is in progress.     * @SUCCEEDED@ - Report creation is complete. You can open the report from the Amazon S3 bucket that you specified when you ran @StartReportCreation@ .     * @FAILED@ - Report creation timed out or the Amazon S3 bucket is not accessible.      * @NO REPORT@ - No report was generated in the last 90 days.
drcrsStatus :: Lens' DescribeReportCreationResponse (Maybe Text)
drcrsStatus = lens _drcrsStatus (\s a -> s {_drcrsStatus = a})

-- | The path to the Amazon S3 bucket where the report was stored on creation.
drcrsS3Location :: Lens' DescribeReportCreationResponse (Maybe Text)
drcrsS3Location = lens _drcrsS3Location (\s a -> s {_drcrsS3Location = a})

-- | Details of the common errors that all operations return.
drcrsErrorMessage :: Lens' DescribeReportCreationResponse (Maybe Text)
drcrsErrorMessage = lens _drcrsErrorMessage (\s a -> s {_drcrsErrorMessage = a})

-- | -- | The response status code.
drcrsResponseStatus :: Lens' DescribeReportCreationResponse Int
drcrsResponseStatus = lens _drcrsResponseStatus (\s a -> s {_drcrsResponseStatus = a})

instance NFData DescribeReportCreationResponse
