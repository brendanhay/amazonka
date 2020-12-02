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
-- Module      : Network.AWS.SageMaker.DescribeExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of an experiment's properties.
module Network.AWS.SageMaker.DescribeExperiment
  ( -- * Creating a Request
    describeExperiment,
    DescribeExperiment,

    -- * Request Lenses
    deExperimentName,

    -- * Destructuring the Response
    describeExperimentResponse,
    DescribeExperimentResponse,

    -- * Response Lenses
    deersCreationTime,
    deersCreatedBy,
    deersLastModifiedTime,
    deersExperimentName,
    deersExperimentARN,
    deersSource,
    deersDisplayName,
    deersLastModifiedBy,
    deersDescription,
    deersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeExperiment' smart constructor.
newtype DescribeExperiment = DescribeExperiment'
  { _deExperimentName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExperiment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deExperimentName' - The name of the experiment to describe.
describeExperiment ::
  -- | 'deExperimentName'
  Text ->
  DescribeExperiment
describeExperiment pExperimentName_ =
  DescribeExperiment' {_deExperimentName = pExperimentName_}

-- | The name of the experiment to describe.
deExperimentName :: Lens' DescribeExperiment Text
deExperimentName = lens _deExperimentName (\s a -> s {_deExperimentName = a})

instance AWSRequest DescribeExperiment where
  type Rs DescribeExperiment = DescribeExperimentResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeExperimentResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "CreatedBy")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "ExperimentName")
            <*> (x .?> "ExperimentArn")
            <*> (x .?> "Source")
            <*> (x .?> "DisplayName")
            <*> (x .?> "LastModifiedBy")
            <*> (x .?> "Description")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeExperiment

instance NFData DescribeExperiment

instance ToHeaders DescribeExperiment where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeExperiment" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeExperiment where
  toJSON DescribeExperiment' {..} =
    object (catMaybes [Just ("ExperimentName" .= _deExperimentName)])

instance ToPath DescribeExperiment where
  toPath = const "/"

instance ToQuery DescribeExperiment where
  toQuery = const mempty

-- | /See:/ 'describeExperimentResponse' smart constructor.
data DescribeExperimentResponse = DescribeExperimentResponse'
  { _deersCreationTime ::
      !(Maybe POSIX),
    _deersCreatedBy ::
      !(Maybe UserContext),
    _deersLastModifiedTime ::
      !(Maybe POSIX),
    _deersExperimentName :: !(Maybe Text),
    _deersExperimentARN :: !(Maybe Text),
    _deersSource ::
      !(Maybe ExperimentSource),
    _deersDisplayName :: !(Maybe Text),
    _deersLastModifiedBy ::
      !(Maybe UserContext),
    _deersDescription :: !(Maybe Text),
    _deersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExperimentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deersCreationTime' - When the experiment was created.
--
-- * 'deersCreatedBy' - Who created the experiment.
--
-- * 'deersLastModifiedTime' - When the experiment was last modified.
--
-- * 'deersExperimentName' - The name of the experiment.
--
-- * 'deersExperimentARN' - The Amazon Resource Name (ARN) of the experiment.
--
-- * 'deersSource' - The ARN of the source and, optionally, the type.
--
-- * 'deersDisplayName' - The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- * 'deersLastModifiedBy' - Who last modified the experiment.
--
-- * 'deersDescription' - The description of the experiment.
--
-- * 'deersResponseStatus' - -- | The response status code.
describeExperimentResponse ::
  -- | 'deersResponseStatus'
  Int ->
  DescribeExperimentResponse
describeExperimentResponse pResponseStatus_ =
  DescribeExperimentResponse'
    { _deersCreationTime = Nothing,
      _deersCreatedBy = Nothing,
      _deersLastModifiedTime = Nothing,
      _deersExperimentName = Nothing,
      _deersExperimentARN = Nothing,
      _deersSource = Nothing,
      _deersDisplayName = Nothing,
      _deersLastModifiedBy = Nothing,
      _deersDescription = Nothing,
      _deersResponseStatus = pResponseStatus_
    }

-- | When the experiment was created.
deersCreationTime :: Lens' DescribeExperimentResponse (Maybe UTCTime)
deersCreationTime = lens _deersCreationTime (\s a -> s {_deersCreationTime = a}) . mapping _Time

-- | Who created the experiment.
deersCreatedBy :: Lens' DescribeExperimentResponse (Maybe UserContext)
deersCreatedBy = lens _deersCreatedBy (\s a -> s {_deersCreatedBy = a})

-- | When the experiment was last modified.
deersLastModifiedTime :: Lens' DescribeExperimentResponse (Maybe UTCTime)
deersLastModifiedTime = lens _deersLastModifiedTime (\s a -> s {_deersLastModifiedTime = a}) . mapping _Time

-- | The name of the experiment.
deersExperimentName :: Lens' DescribeExperimentResponse (Maybe Text)
deersExperimentName = lens _deersExperimentName (\s a -> s {_deersExperimentName = a})

-- | The Amazon Resource Name (ARN) of the experiment.
deersExperimentARN :: Lens' DescribeExperimentResponse (Maybe Text)
deersExperimentARN = lens _deersExperimentARN (\s a -> s {_deersExperimentARN = a})

-- | The ARN of the source and, optionally, the type.
deersSource :: Lens' DescribeExperimentResponse (Maybe ExperimentSource)
deersSource = lens _deersSource (\s a -> s {_deersSource = a})

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
deersDisplayName :: Lens' DescribeExperimentResponse (Maybe Text)
deersDisplayName = lens _deersDisplayName (\s a -> s {_deersDisplayName = a})

-- | Who last modified the experiment.
deersLastModifiedBy :: Lens' DescribeExperimentResponse (Maybe UserContext)
deersLastModifiedBy = lens _deersLastModifiedBy (\s a -> s {_deersLastModifiedBy = a})

-- | The description of the experiment.
deersDescription :: Lens' DescribeExperimentResponse (Maybe Text)
deersDescription = lens _deersDescription (\s a -> s {_deersDescription = a})

-- | -- | The response status code.
deersResponseStatus :: Lens' DescribeExperimentResponse Int
deersResponseStatus = lens _deersResponseStatus (\s a -> s {_deersResponseStatus = a})

instance NFData DescribeExperimentResponse
