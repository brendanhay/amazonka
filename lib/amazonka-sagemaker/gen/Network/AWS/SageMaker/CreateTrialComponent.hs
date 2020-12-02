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
-- Module      : Network.AWS.SageMaker.CreateTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a /trial component/ , which is a stage of a machine learning /trial/ . A trial is composed of one or more trial components. A trial component can be used in multiple trials.
--
--
-- Trial components include pre-processing jobs, training jobs, and batch transform jobs.
--
-- When you use Amazon SageMaker Studio or the Amazon SageMaker Python SDK, all experiments, trials, and trial components are automatically tracked, logged, and indexed. When you use the AWS SDK for Python (Boto), you must use the logging APIs provided by the SDK.
--
-- You can add tags to a trial component and then use the 'Search' API to search for the tags.
module Network.AWS.SageMaker.CreateTrialComponent
  ( -- * Creating a Request
    createTrialComponent,
    CreateTrialComponent,

    -- * Request Lenses
    ctcStatus,
    ctcOutputArtifacts,
    ctcStartTime,
    ctcEndTime,
    ctcParameters,
    ctcDisplayName,
    ctcInputArtifacts,
    ctcTags,
    ctcTrialComponentName,

    -- * Destructuring the Response
    createTrialComponentResponse,
    CreateTrialComponentResponse,

    -- * Response Lenses
    ctcrsTrialComponentARN,
    ctcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createTrialComponent' smart constructor.
data CreateTrialComponent = CreateTrialComponent'
  { _ctcStatus ::
      !(Maybe TrialComponentStatus),
    _ctcOutputArtifacts ::
      !(Maybe (Map Text (TrialComponentArtifact))),
    _ctcStartTime :: !(Maybe POSIX),
    _ctcEndTime :: !(Maybe POSIX),
    _ctcParameters ::
      !( Maybe
           (Map Text (TrialComponentParameterValue))
       ),
    _ctcDisplayName :: !(Maybe Text),
    _ctcInputArtifacts ::
      !(Maybe (Map Text (TrialComponentArtifact))),
    _ctcTags :: !(Maybe [Tag]),
    _ctcTrialComponentName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrialComponent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctcStatus' - The status of the component. States include:     * InProgress     * Completed     * Failed
--
-- * 'ctcOutputArtifacts' - The output artifacts for the component. Examples of output artifacts are metrics, snapshots, logs, and images.
--
-- * 'ctcStartTime' - When the component started.
--
-- * 'ctcEndTime' - When the component ended.
--
-- * 'ctcParameters' - The hyperparameters for the component.
--
-- * 'ctcDisplayName' - The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- * 'ctcInputArtifacts' - The input artifacts for the component. Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types.
--
-- * 'ctcTags' - A list of tags to associate with the component. You can use 'Search' API to search on the tags.
--
-- * 'ctcTrialComponentName' - The name of the component. The name must be unique in your AWS account and is not case-sensitive.
createTrialComponent ::
  -- | 'ctcTrialComponentName'
  Text ->
  CreateTrialComponent
createTrialComponent pTrialComponentName_ =
  CreateTrialComponent'
    { _ctcStatus = Nothing,
      _ctcOutputArtifacts = Nothing,
      _ctcStartTime = Nothing,
      _ctcEndTime = Nothing,
      _ctcParameters = Nothing,
      _ctcDisplayName = Nothing,
      _ctcInputArtifacts = Nothing,
      _ctcTags = Nothing,
      _ctcTrialComponentName = pTrialComponentName_
    }

-- | The status of the component. States include:     * InProgress     * Completed     * Failed
ctcStatus :: Lens' CreateTrialComponent (Maybe TrialComponentStatus)
ctcStatus = lens _ctcStatus (\s a -> s {_ctcStatus = a})

-- | The output artifacts for the component. Examples of output artifacts are metrics, snapshots, logs, and images.
ctcOutputArtifacts :: Lens' CreateTrialComponent (HashMap Text (TrialComponentArtifact))
ctcOutputArtifacts = lens _ctcOutputArtifacts (\s a -> s {_ctcOutputArtifacts = a}) . _Default . _Map

-- | When the component started.
ctcStartTime :: Lens' CreateTrialComponent (Maybe UTCTime)
ctcStartTime = lens _ctcStartTime (\s a -> s {_ctcStartTime = a}) . mapping _Time

-- | When the component ended.
ctcEndTime :: Lens' CreateTrialComponent (Maybe UTCTime)
ctcEndTime = lens _ctcEndTime (\s a -> s {_ctcEndTime = a}) . mapping _Time

-- | The hyperparameters for the component.
ctcParameters :: Lens' CreateTrialComponent (HashMap Text (TrialComponentParameterValue))
ctcParameters = lens _ctcParameters (\s a -> s {_ctcParameters = a}) . _Default . _Map

-- | The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
ctcDisplayName :: Lens' CreateTrialComponent (Maybe Text)
ctcDisplayName = lens _ctcDisplayName (\s a -> s {_ctcDisplayName = a})

-- | The input artifacts for the component. Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types.
ctcInputArtifacts :: Lens' CreateTrialComponent (HashMap Text (TrialComponentArtifact))
ctcInputArtifacts = lens _ctcInputArtifacts (\s a -> s {_ctcInputArtifacts = a}) . _Default . _Map

-- | A list of tags to associate with the component. You can use 'Search' API to search on the tags.
ctcTags :: Lens' CreateTrialComponent [Tag]
ctcTags = lens _ctcTags (\s a -> s {_ctcTags = a}) . _Default . _Coerce

-- | The name of the component. The name must be unique in your AWS account and is not case-sensitive.
ctcTrialComponentName :: Lens' CreateTrialComponent Text
ctcTrialComponentName = lens _ctcTrialComponentName (\s a -> s {_ctcTrialComponentName = a})

instance AWSRequest CreateTrialComponent where
  type Rs CreateTrialComponent = CreateTrialComponentResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateTrialComponentResponse'
            <$> (x .?> "TrialComponentArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateTrialComponent

instance NFData CreateTrialComponent

instance ToHeaders CreateTrialComponent where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.CreateTrialComponent" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTrialComponent where
  toJSON CreateTrialComponent' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _ctcStatus,
            ("OutputArtifacts" .=) <$> _ctcOutputArtifacts,
            ("StartTime" .=) <$> _ctcStartTime,
            ("EndTime" .=) <$> _ctcEndTime,
            ("Parameters" .=) <$> _ctcParameters,
            ("DisplayName" .=) <$> _ctcDisplayName,
            ("InputArtifacts" .=) <$> _ctcInputArtifacts,
            ("Tags" .=) <$> _ctcTags,
            Just ("TrialComponentName" .= _ctcTrialComponentName)
          ]
      )

instance ToPath CreateTrialComponent where
  toPath = const "/"

instance ToQuery CreateTrialComponent where
  toQuery = const mempty

-- | /See:/ 'createTrialComponentResponse' smart constructor.
data CreateTrialComponentResponse = CreateTrialComponentResponse'
  { _ctcrsTrialComponentARN ::
      !(Maybe Text),
    _ctcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrialComponentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctcrsTrialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
--
-- * 'ctcrsResponseStatus' - -- | The response status code.
createTrialComponentResponse ::
  -- | 'ctcrsResponseStatus'
  Int ->
  CreateTrialComponentResponse
createTrialComponentResponse pResponseStatus_ =
  CreateTrialComponentResponse'
    { _ctcrsTrialComponentARN = Nothing,
      _ctcrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial component.
ctcrsTrialComponentARN :: Lens' CreateTrialComponentResponse (Maybe Text)
ctcrsTrialComponentARN = lens _ctcrsTrialComponentARN (\s a -> s {_ctcrsTrialComponentARN = a})

-- | -- | The response status code.
ctcrsResponseStatus :: Lens' CreateTrialComponentResponse Int
ctcrsResponseStatus = lens _ctcrsResponseStatus (\s a -> s {_ctcrsResponseStatus = a})

instance NFData CreateTrialComponentResponse
