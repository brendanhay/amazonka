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
-- Module      : Network.AWS.SageMaker.DescribeTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trials component's properties.
module Network.AWS.SageMaker.DescribeTrialComponent
  ( -- * Creating a Request
    describeTrialComponent,
    DescribeTrialComponent,

    -- * Request Lenses
    desTrialComponentName,

    -- * Destructuring the Response
    describeTrialComponentResponse,
    DescribeTrialComponentResponse,

    -- * Response Lenses
    desersCreationTime,
    desersStatus,
    desersMetrics,
    desersOutputArtifacts,
    desersStartTime,
    desersCreatedBy,
    desersLastModifiedTime,
    desersEndTime,
    desersTrialComponentName,
    desersParameters,
    desersSource,
    desersDisplayName,
    desersLastModifiedBy,
    desersTrialComponentARN,
    desersInputArtifacts,
    desersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeTrialComponent' smart constructor.
newtype DescribeTrialComponent = DescribeTrialComponent'
  { _desTrialComponentName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrialComponent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desTrialComponentName' - The name of the trial component to describe.
describeTrialComponent ::
  -- | 'desTrialComponentName'
  Text ->
  DescribeTrialComponent
describeTrialComponent pTrialComponentName_ =
  DescribeTrialComponent'
    { _desTrialComponentName =
        pTrialComponentName_
    }

-- | The name of the trial component to describe.
desTrialComponentName :: Lens' DescribeTrialComponent Text
desTrialComponentName = lens _desTrialComponentName (\s a -> s {_desTrialComponentName = a})

instance AWSRequest DescribeTrialComponent where
  type Rs DescribeTrialComponent = DescribeTrialComponentResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeTrialComponentResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "Status")
            <*> (x .?> "Metrics" .!@ mempty)
            <*> (x .?> "OutputArtifacts" .!@ mempty)
            <*> (x .?> "StartTime")
            <*> (x .?> "CreatedBy")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "EndTime")
            <*> (x .?> "TrialComponentName")
            <*> (x .?> "Parameters" .!@ mempty)
            <*> (x .?> "Source")
            <*> (x .?> "DisplayName")
            <*> (x .?> "LastModifiedBy")
            <*> (x .?> "TrialComponentArn")
            <*> (x .?> "InputArtifacts" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTrialComponent

instance NFData DescribeTrialComponent

instance ToHeaders DescribeTrialComponent where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DescribeTrialComponent" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeTrialComponent where
  toJSON DescribeTrialComponent' {..} =
    object
      (catMaybes [Just ("TrialComponentName" .= _desTrialComponentName)])

instance ToPath DescribeTrialComponent where
  toPath = const "/"

instance ToQuery DescribeTrialComponent where
  toQuery = const mempty

-- | /See:/ 'describeTrialComponentResponse' smart constructor.
data DescribeTrialComponentResponse = DescribeTrialComponentResponse'
  { _desersCreationTime ::
      !(Maybe POSIX),
    _desersStatus ::
      !(Maybe TrialComponentStatus),
    _desersMetrics ::
      !( Maybe
           [TrialComponentMetricSummary]
       ),
    _desersOutputArtifacts ::
      !( Maybe
           ( Map
               Text
               (TrialComponentArtifact)
           )
       ),
    _desersStartTime ::
      !(Maybe POSIX),
    _desersCreatedBy ::
      !(Maybe UserContext),
    _desersLastModifiedTime ::
      !(Maybe POSIX),
    _desersEndTime ::
      !(Maybe POSIX),
    _desersTrialComponentName ::
      !(Maybe Text),
    _desersParameters ::
      !( Maybe
           ( Map
               Text
               (TrialComponentParameterValue)
           )
       ),
    _desersSource ::
      !(Maybe TrialComponentSource),
    _desersDisplayName ::
      !(Maybe Text),
    _desersLastModifiedBy ::
      !(Maybe UserContext),
    _desersTrialComponentARN ::
      !(Maybe Text),
    _desersInputArtifacts ::
      !( Maybe
           ( Map
               Text
               (TrialComponentArtifact)
           )
       ),
    _desersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrialComponentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desersCreationTime' - When the component was created.
--
-- * 'desersStatus' - The status of the component. States include:     * InProgress     * Completed     * Failed
--
-- * 'desersMetrics' - The metrics for the component.
--
-- * 'desersOutputArtifacts' - The output artifacts of the component.
--
-- * 'desersStartTime' - When the component started.
--
-- * 'desersCreatedBy' - Who created the component.
--
-- * 'desersLastModifiedTime' - When the component was last modified.
--
-- * 'desersEndTime' - When the component ended.
--
-- * 'desersTrialComponentName' - The name of the trial component.
--
-- * 'desersParameters' - The hyperparameters of the component.
--
-- * 'desersSource' - The Amazon Resource Name (ARN) of the source and, optionally, the job type.
--
-- * 'desersDisplayName' - The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- * 'desersLastModifiedBy' - Who last modified the component.
--
-- * 'desersTrialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
--
-- * 'desersInputArtifacts' - The input artifacts of the component.
--
-- * 'desersResponseStatus' - -- | The response status code.
describeTrialComponentResponse ::
  -- | 'desersResponseStatus'
  Int ->
  DescribeTrialComponentResponse
describeTrialComponentResponse pResponseStatus_ =
  DescribeTrialComponentResponse'
    { _desersCreationTime = Nothing,
      _desersStatus = Nothing,
      _desersMetrics = Nothing,
      _desersOutputArtifacts = Nothing,
      _desersStartTime = Nothing,
      _desersCreatedBy = Nothing,
      _desersLastModifiedTime = Nothing,
      _desersEndTime = Nothing,
      _desersTrialComponentName = Nothing,
      _desersParameters = Nothing,
      _desersSource = Nothing,
      _desersDisplayName = Nothing,
      _desersLastModifiedBy = Nothing,
      _desersTrialComponentARN = Nothing,
      _desersInputArtifacts = Nothing,
      _desersResponseStatus = pResponseStatus_
    }

-- | When the component was created.
desersCreationTime :: Lens' DescribeTrialComponentResponse (Maybe UTCTime)
desersCreationTime = lens _desersCreationTime (\s a -> s {_desersCreationTime = a}) . mapping _Time

-- | The status of the component. States include:     * InProgress     * Completed     * Failed
desersStatus :: Lens' DescribeTrialComponentResponse (Maybe TrialComponentStatus)
desersStatus = lens _desersStatus (\s a -> s {_desersStatus = a})

-- | The metrics for the component.
desersMetrics :: Lens' DescribeTrialComponentResponse [TrialComponentMetricSummary]
desersMetrics = lens _desersMetrics (\s a -> s {_desersMetrics = a}) . _Default . _Coerce

-- | The output artifacts of the component.
desersOutputArtifacts :: Lens' DescribeTrialComponentResponse (HashMap Text (TrialComponentArtifact))
desersOutputArtifacts = lens _desersOutputArtifacts (\s a -> s {_desersOutputArtifacts = a}) . _Default . _Map

-- | When the component started.
desersStartTime :: Lens' DescribeTrialComponentResponse (Maybe UTCTime)
desersStartTime = lens _desersStartTime (\s a -> s {_desersStartTime = a}) . mapping _Time

-- | Who created the component.
desersCreatedBy :: Lens' DescribeTrialComponentResponse (Maybe UserContext)
desersCreatedBy = lens _desersCreatedBy (\s a -> s {_desersCreatedBy = a})

-- | When the component was last modified.
desersLastModifiedTime :: Lens' DescribeTrialComponentResponse (Maybe UTCTime)
desersLastModifiedTime = lens _desersLastModifiedTime (\s a -> s {_desersLastModifiedTime = a}) . mapping _Time

-- | When the component ended.
desersEndTime :: Lens' DescribeTrialComponentResponse (Maybe UTCTime)
desersEndTime = lens _desersEndTime (\s a -> s {_desersEndTime = a}) . mapping _Time

-- | The name of the trial component.
desersTrialComponentName :: Lens' DescribeTrialComponentResponse (Maybe Text)
desersTrialComponentName = lens _desersTrialComponentName (\s a -> s {_desersTrialComponentName = a})

-- | The hyperparameters of the component.
desersParameters :: Lens' DescribeTrialComponentResponse (HashMap Text (TrialComponentParameterValue))
desersParameters = lens _desersParameters (\s a -> s {_desersParameters = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
desersSource :: Lens' DescribeTrialComponentResponse (Maybe TrialComponentSource)
desersSource = lens _desersSource (\s a -> s {_desersSource = a})

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
desersDisplayName :: Lens' DescribeTrialComponentResponse (Maybe Text)
desersDisplayName = lens _desersDisplayName (\s a -> s {_desersDisplayName = a})

-- | Who last modified the component.
desersLastModifiedBy :: Lens' DescribeTrialComponentResponse (Maybe UserContext)
desersLastModifiedBy = lens _desersLastModifiedBy (\s a -> s {_desersLastModifiedBy = a})

-- | The Amazon Resource Name (ARN) of the trial component.
desersTrialComponentARN :: Lens' DescribeTrialComponentResponse (Maybe Text)
desersTrialComponentARN = lens _desersTrialComponentARN (\s a -> s {_desersTrialComponentARN = a})

-- | The input artifacts of the component.
desersInputArtifacts :: Lens' DescribeTrialComponentResponse (HashMap Text (TrialComponentArtifact))
desersInputArtifacts = lens _desersInputArtifacts (\s a -> s {_desersInputArtifacts = a}) . _Default . _Map

-- | -- | The response status code.
desersResponseStatus :: Lens' DescribeTrialComponentResponse Int
desersResponseStatus = lens _desersResponseStatus (\s a -> s {_desersResponseStatus = a})

instance NFData DescribeTrialComponentResponse
