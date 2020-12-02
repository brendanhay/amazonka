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
-- Module      : Network.AWS.SageMaker.CreateTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon SageMaker /trial/ . A trial is a set of steps called /trial components/ that produce a machine learning model. A trial is part of a single Amazon SageMaker /experiment/ .
--
--
-- When you use Amazon SageMaker Studio or the Amazon SageMaker Python SDK, all experiments, trials, and trial components are automatically tracked, logged, and indexed. When you use the AWS SDK for Python (Boto), you must use the logging APIs provided by the SDK.
--
-- You can add tags to a trial and then use the 'Search' API to search for the tags.
--
-- To get a list of all your trials, call the 'ListTrials' API. To view a trial's properties, call the 'DescribeTrial' API. To create a trial component, call the 'CreateTrialComponent' API.
module Network.AWS.SageMaker.CreateTrial
  ( -- * Creating a Request
    createTrial,
    CreateTrial,

    -- * Request Lenses
    ctDisplayName,
    ctTags,
    ctTrialName,
    ctExperimentName,

    -- * Destructuring the Response
    createTrialResponse,
    CreateTrialResponse,

    -- * Response Lenses
    ctrsTrialARN,
    ctrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createTrial' smart constructor.
data CreateTrial = CreateTrial'
  { _ctDisplayName :: !(Maybe Text),
    _ctTags :: !(Maybe [Tag]),
    _ctTrialName :: !Text,
    _ctExperimentName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrial' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctDisplayName' - The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- * 'ctTags' - A list of tags to associate with the trial. You can use 'Search' API to search on the tags.
--
-- * 'ctTrialName' - The name of the trial. The name must be unique in your AWS account and is not case-sensitive.
--
-- * 'ctExperimentName' - The name of the experiment to associate the trial with.
createTrial ::
  -- | 'ctTrialName'
  Text ->
  -- | 'ctExperimentName'
  Text ->
  CreateTrial
createTrial pTrialName_ pExperimentName_ =
  CreateTrial'
    { _ctDisplayName = Nothing,
      _ctTags = Nothing,
      _ctTrialName = pTrialName_,
      _ctExperimentName = pExperimentName_
    }

-- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
ctDisplayName :: Lens' CreateTrial (Maybe Text)
ctDisplayName = lens _ctDisplayName (\s a -> s {_ctDisplayName = a})

-- | A list of tags to associate with the trial. You can use 'Search' API to search on the tags.
ctTags :: Lens' CreateTrial [Tag]
ctTags = lens _ctTags (\s a -> s {_ctTags = a}) . _Default . _Coerce

-- | The name of the trial. The name must be unique in your AWS account and is not case-sensitive.
ctTrialName :: Lens' CreateTrial Text
ctTrialName = lens _ctTrialName (\s a -> s {_ctTrialName = a})

-- | The name of the experiment to associate the trial with.
ctExperimentName :: Lens' CreateTrial Text
ctExperimentName = lens _ctExperimentName (\s a -> s {_ctExperimentName = a})

instance AWSRequest CreateTrial where
  type Rs CreateTrial = CreateTrialResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateTrialResponse'
            <$> (x .?> "TrialArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateTrial

instance NFData CreateTrial

instance ToHeaders CreateTrial where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateTrial" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTrial where
  toJSON CreateTrial' {..} =
    object
      ( catMaybes
          [ ("DisplayName" .=) <$> _ctDisplayName,
            ("Tags" .=) <$> _ctTags,
            Just ("TrialName" .= _ctTrialName),
            Just ("ExperimentName" .= _ctExperimentName)
          ]
      )

instance ToPath CreateTrial where
  toPath = const "/"

instance ToQuery CreateTrial where
  toQuery = const mempty

-- | /See:/ 'createTrialResponse' smart constructor.
data CreateTrialResponse = CreateTrialResponse'
  { _ctrsTrialARN ::
      !(Maybe Text),
    _ctrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrialResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsTrialARN' - The Amazon Resource Name (ARN) of the trial.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTrialResponse ::
  -- | 'ctrsResponseStatus'
  Int ->
  CreateTrialResponse
createTrialResponse pResponseStatus_ =
  CreateTrialResponse'
    { _ctrsTrialARN = Nothing,
      _ctrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
ctrsTrialARN :: Lens' CreateTrialResponse (Maybe Text)
ctrsTrialARN = lens _ctrsTrialARN (\s a -> s {_ctrsTrialARN = a})

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTrialResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\s a -> s {_ctrsResponseStatus = a})

instance NFData CreateTrialResponse
