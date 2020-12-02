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
-- Module      : Network.AWS.SageMaker.DescribeTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trial's properties.
module Network.AWS.SageMaker.DescribeTrial
  ( -- * Creating a Request
    describeTrial,
    DescribeTrial,

    -- * Request Lenses
    dtTrialName,

    -- * Destructuring the Response
    describeTrialResponse,
    DescribeTrialResponse,

    -- * Response Lenses
    dtrsCreationTime,
    dtrsTrialARN,
    dtrsCreatedBy,
    dtrsLastModifiedTime,
    dtrsExperimentName,
    dtrsSource,
    dtrsDisplayName,
    dtrsTrialName,
    dtrsLastModifiedBy,
    dtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeTrial' smart constructor.
newtype DescribeTrial = DescribeTrial' {_dtTrialName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrial' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTrialName' - The name of the trial to describe.
describeTrial ::
  -- | 'dtTrialName'
  Text ->
  DescribeTrial
describeTrial pTrialName_ =
  DescribeTrial' {_dtTrialName = pTrialName_}

-- | The name of the trial to describe.
dtTrialName :: Lens' DescribeTrial Text
dtTrialName = lens _dtTrialName (\s a -> s {_dtTrialName = a})

instance AWSRequest DescribeTrial where
  type Rs DescribeTrial = DescribeTrialResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeTrialResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "TrialArn")
            <*> (x .?> "CreatedBy")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "ExperimentName")
            <*> (x .?> "Source")
            <*> (x .?> "DisplayName")
            <*> (x .?> "TrialName")
            <*> (x .?> "LastModifiedBy")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTrial

instance NFData DescribeTrial

instance ToHeaders DescribeTrial where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeTrial" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeTrial where
  toJSON DescribeTrial' {..} =
    object (catMaybes [Just ("TrialName" .= _dtTrialName)])

instance ToPath DescribeTrial where
  toPath = const "/"

instance ToQuery DescribeTrial where
  toQuery = const mempty

-- | /See:/ 'describeTrialResponse' smart constructor.
data DescribeTrialResponse = DescribeTrialResponse'
  { _dtrsCreationTime ::
      !(Maybe POSIX),
    _dtrsTrialARN :: !(Maybe Text),
    _dtrsCreatedBy :: !(Maybe UserContext),
    _dtrsLastModifiedTime :: !(Maybe POSIX),
    _dtrsExperimentName :: !(Maybe Text),
    _dtrsSource :: !(Maybe TrialSource),
    _dtrsDisplayName :: !(Maybe Text),
    _dtrsTrialName :: !(Maybe Text),
    _dtrsLastModifiedBy :: !(Maybe UserContext),
    _dtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrialResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsCreationTime' - When the trial was created.
--
-- * 'dtrsTrialARN' - The Amazon Resource Name (ARN) of the trial.
--
-- * 'dtrsCreatedBy' - Who created the trial.
--
-- * 'dtrsLastModifiedTime' - When the trial was last modified.
--
-- * 'dtrsExperimentName' - The name of the experiment the trial is part of.
--
-- * 'dtrsSource' - The Amazon Resource Name (ARN) of the source and, optionally, the job type.
--
-- * 'dtrsDisplayName' - The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- * 'dtrsTrialName' - The name of the trial.
--
-- * 'dtrsLastModifiedBy' - Who last modified the trial.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
describeTrialResponse ::
  -- | 'dtrsResponseStatus'
  Int ->
  DescribeTrialResponse
describeTrialResponse pResponseStatus_ =
  DescribeTrialResponse'
    { _dtrsCreationTime = Nothing,
      _dtrsTrialARN = Nothing,
      _dtrsCreatedBy = Nothing,
      _dtrsLastModifiedTime = Nothing,
      _dtrsExperimentName = Nothing,
      _dtrsSource = Nothing,
      _dtrsDisplayName = Nothing,
      _dtrsTrialName = Nothing,
      _dtrsLastModifiedBy = Nothing,
      _dtrsResponseStatus = pResponseStatus_
    }

-- | When the trial was created.
dtrsCreationTime :: Lens' DescribeTrialResponse (Maybe UTCTime)
dtrsCreationTime = lens _dtrsCreationTime (\s a -> s {_dtrsCreationTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the trial.
dtrsTrialARN :: Lens' DescribeTrialResponse (Maybe Text)
dtrsTrialARN = lens _dtrsTrialARN (\s a -> s {_dtrsTrialARN = a})

-- | Who created the trial.
dtrsCreatedBy :: Lens' DescribeTrialResponse (Maybe UserContext)
dtrsCreatedBy = lens _dtrsCreatedBy (\s a -> s {_dtrsCreatedBy = a})

-- | When the trial was last modified.
dtrsLastModifiedTime :: Lens' DescribeTrialResponse (Maybe UTCTime)
dtrsLastModifiedTime = lens _dtrsLastModifiedTime (\s a -> s {_dtrsLastModifiedTime = a}) . mapping _Time

-- | The name of the experiment the trial is part of.
dtrsExperimentName :: Lens' DescribeTrialResponse (Maybe Text)
dtrsExperimentName = lens _dtrsExperimentName (\s a -> s {_dtrsExperimentName = a})

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
dtrsSource :: Lens' DescribeTrialResponse (Maybe TrialSource)
dtrsSource = lens _dtrsSource (\s a -> s {_dtrsSource = a})

-- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
dtrsDisplayName :: Lens' DescribeTrialResponse (Maybe Text)
dtrsDisplayName = lens _dtrsDisplayName (\s a -> s {_dtrsDisplayName = a})

-- | The name of the trial.
dtrsTrialName :: Lens' DescribeTrialResponse (Maybe Text)
dtrsTrialName = lens _dtrsTrialName (\s a -> s {_dtrsTrialName = a})

-- | Who last modified the trial.
dtrsLastModifiedBy :: Lens' DescribeTrialResponse (Maybe UserContext)
dtrsLastModifiedBy = lens _dtrsLastModifiedBy (\s a -> s {_dtrsLastModifiedBy = a})

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DescribeTrialResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\s a -> s {_dtrsResponseStatus = a})

instance NFData DescribeTrialResponse
