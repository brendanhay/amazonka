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
-- Module      : Network.AWS.SageMaker.UpdateTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more properties of a trial component.
module Network.AWS.SageMaker.UpdateTrialComponent
  ( -- * Creating a Request
    updateTrialComponent,
    UpdateTrialComponent,

    -- * Request Lenses
    utcStatus,
    utcParametersToRemove,
    utcOutputArtifacts,
    utcStartTime,
    utcOutputArtifactsToRemove,
    utcEndTime,
    utcParameters,
    utcDisplayName,
    utcInputArtifacts,
    utcInputArtifactsToRemove,
    utcTrialComponentName,

    -- * Destructuring the Response
    updateTrialComponentResponse,
    UpdateTrialComponentResponse,

    -- * Response Lenses
    utcrsTrialComponentARN,
    utcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'updateTrialComponent' smart constructor.
data UpdateTrialComponent = UpdateTrialComponent'
  { _utcStatus ::
      !(Maybe TrialComponentStatus),
    _utcParametersToRemove :: !(Maybe [Text]),
    _utcOutputArtifacts ::
      !(Maybe (Map Text (TrialComponentArtifact))),
    _utcStartTime :: !(Maybe POSIX),
    _utcOutputArtifactsToRemove :: !(Maybe [Text]),
    _utcEndTime :: !(Maybe POSIX),
    _utcParameters ::
      !( Maybe
           (Map Text (TrialComponentParameterValue))
       ),
    _utcDisplayName :: !(Maybe Text),
    _utcInputArtifacts ::
      !(Maybe (Map Text (TrialComponentArtifact))),
    _utcInputArtifactsToRemove :: !(Maybe [Text]),
    _utcTrialComponentName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTrialComponent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utcStatus' - The new status of the component.
--
-- * 'utcParametersToRemove' - The hyperparameters to remove from the component.
--
-- * 'utcOutputArtifacts' - Replaces all of the component's output artifacts with the specified artifacts.
--
-- * 'utcStartTime' - When the component started.
--
-- * 'utcOutputArtifactsToRemove' - The output artifacts to remove from the component.
--
-- * 'utcEndTime' - When the component ended.
--
-- * 'utcParameters' - Replaces all of the component's hyperparameters with the specified hyperparameters.
--
-- * 'utcDisplayName' - The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- * 'utcInputArtifacts' - Replaces all of the component's input artifacts with the specified artifacts.
--
-- * 'utcInputArtifactsToRemove' - The input artifacts to remove from the component.
--
-- * 'utcTrialComponentName' - The name of the component to update.
updateTrialComponent ::
  -- | 'utcTrialComponentName'
  Text ->
  UpdateTrialComponent
updateTrialComponent pTrialComponentName_ =
  UpdateTrialComponent'
    { _utcStatus = Nothing,
      _utcParametersToRemove = Nothing,
      _utcOutputArtifacts = Nothing,
      _utcStartTime = Nothing,
      _utcOutputArtifactsToRemove = Nothing,
      _utcEndTime = Nothing,
      _utcParameters = Nothing,
      _utcDisplayName = Nothing,
      _utcInputArtifacts = Nothing,
      _utcInputArtifactsToRemove = Nothing,
      _utcTrialComponentName = pTrialComponentName_
    }

-- | The new status of the component.
utcStatus :: Lens' UpdateTrialComponent (Maybe TrialComponentStatus)
utcStatus = lens _utcStatus (\s a -> s {_utcStatus = a})

-- | The hyperparameters to remove from the component.
utcParametersToRemove :: Lens' UpdateTrialComponent [Text]
utcParametersToRemove = lens _utcParametersToRemove (\s a -> s {_utcParametersToRemove = a}) . _Default . _Coerce

-- | Replaces all of the component's output artifacts with the specified artifacts.
utcOutputArtifacts :: Lens' UpdateTrialComponent (HashMap Text (TrialComponentArtifact))
utcOutputArtifacts = lens _utcOutputArtifacts (\s a -> s {_utcOutputArtifacts = a}) . _Default . _Map

-- | When the component started.
utcStartTime :: Lens' UpdateTrialComponent (Maybe UTCTime)
utcStartTime = lens _utcStartTime (\s a -> s {_utcStartTime = a}) . mapping _Time

-- | The output artifacts to remove from the component.
utcOutputArtifactsToRemove :: Lens' UpdateTrialComponent [Text]
utcOutputArtifactsToRemove = lens _utcOutputArtifactsToRemove (\s a -> s {_utcOutputArtifactsToRemove = a}) . _Default . _Coerce

-- | When the component ended.
utcEndTime :: Lens' UpdateTrialComponent (Maybe UTCTime)
utcEndTime = lens _utcEndTime (\s a -> s {_utcEndTime = a}) . mapping _Time

-- | Replaces all of the component's hyperparameters with the specified hyperparameters.
utcParameters :: Lens' UpdateTrialComponent (HashMap Text (TrialComponentParameterValue))
utcParameters = lens _utcParameters (\s a -> s {_utcParameters = a}) . _Default . _Map

-- | The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
utcDisplayName :: Lens' UpdateTrialComponent (Maybe Text)
utcDisplayName = lens _utcDisplayName (\s a -> s {_utcDisplayName = a})

-- | Replaces all of the component's input artifacts with the specified artifacts.
utcInputArtifacts :: Lens' UpdateTrialComponent (HashMap Text (TrialComponentArtifact))
utcInputArtifacts = lens _utcInputArtifacts (\s a -> s {_utcInputArtifacts = a}) . _Default . _Map

-- | The input artifacts to remove from the component.
utcInputArtifactsToRemove :: Lens' UpdateTrialComponent [Text]
utcInputArtifactsToRemove = lens _utcInputArtifactsToRemove (\s a -> s {_utcInputArtifactsToRemove = a}) . _Default . _Coerce

-- | The name of the component to update.
utcTrialComponentName :: Lens' UpdateTrialComponent Text
utcTrialComponentName = lens _utcTrialComponentName (\s a -> s {_utcTrialComponentName = a})

instance AWSRequest UpdateTrialComponent where
  type Rs UpdateTrialComponent = UpdateTrialComponentResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          UpdateTrialComponentResponse'
            <$> (x .?> "TrialComponentArn") <*> (pure (fromEnum s))
      )

instance Hashable UpdateTrialComponent

instance NFData UpdateTrialComponent

instance ToHeaders UpdateTrialComponent where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.UpdateTrialComponent" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateTrialComponent where
  toJSON UpdateTrialComponent' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _utcStatus,
            ("ParametersToRemove" .=) <$> _utcParametersToRemove,
            ("OutputArtifacts" .=) <$> _utcOutputArtifacts,
            ("StartTime" .=) <$> _utcStartTime,
            ("OutputArtifactsToRemove" .=) <$> _utcOutputArtifactsToRemove,
            ("EndTime" .=) <$> _utcEndTime,
            ("Parameters" .=) <$> _utcParameters,
            ("DisplayName" .=) <$> _utcDisplayName,
            ("InputArtifacts" .=) <$> _utcInputArtifacts,
            ("InputArtifactsToRemove" .=) <$> _utcInputArtifactsToRemove,
            Just ("TrialComponentName" .= _utcTrialComponentName)
          ]
      )

instance ToPath UpdateTrialComponent where
  toPath = const "/"

instance ToQuery UpdateTrialComponent where
  toQuery = const mempty

-- | /See:/ 'updateTrialComponentResponse' smart constructor.
data UpdateTrialComponentResponse = UpdateTrialComponentResponse'
  { _utcrsTrialComponentARN ::
      !(Maybe Text),
    _utcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTrialComponentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utcrsTrialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
--
-- * 'utcrsResponseStatus' - -- | The response status code.
updateTrialComponentResponse ::
  -- | 'utcrsResponseStatus'
  Int ->
  UpdateTrialComponentResponse
updateTrialComponentResponse pResponseStatus_ =
  UpdateTrialComponentResponse'
    { _utcrsTrialComponentARN = Nothing,
      _utcrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial component.
utcrsTrialComponentARN :: Lens' UpdateTrialComponentResponse (Maybe Text)
utcrsTrialComponentARN = lens _utcrsTrialComponentARN (\s a -> s {_utcrsTrialComponentARN = a})

-- | -- | The response status code.
utcrsResponseStatus :: Lens' UpdateTrialComponentResponse Int
utcrsResponseStatus = lens _utcrsResponseStatus (\s a -> s {_utcrsResponseStatus = a})

instance NFData UpdateTrialComponentResponse
