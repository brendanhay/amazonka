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
-- Module      : Network.AWS.SageMaker.UpdateExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, updates, or removes the description of an experiment. Updates the display name of an experiment.
module Network.AWS.SageMaker.UpdateExperiment
  ( -- * Creating a Request
    updateExperiment,
    UpdateExperiment,

    -- * Request Lenses
    ueDisplayName,
    ueDescription,
    ueExperimentName,

    -- * Destructuring the Response
    updateExperimentResponse,
    UpdateExperimentResponse,

    -- * Response Lenses
    ursExperimentARN,
    ursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'updateExperiment' smart constructor.
data UpdateExperiment = UpdateExperiment'
  { _ueDisplayName ::
      !(Maybe Text),
    _ueDescription :: !(Maybe Text),
    _ueExperimentName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateExperiment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueDisplayName' - The name of the experiment as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- * 'ueDescription' - The description of the experiment.
--
-- * 'ueExperimentName' - The name of the experiment to update.
updateExperiment ::
  -- | 'ueExperimentName'
  Text ->
  UpdateExperiment
updateExperiment pExperimentName_ =
  UpdateExperiment'
    { _ueDisplayName = Nothing,
      _ueDescription = Nothing,
      _ueExperimentName = pExperimentName_
    }

-- | The name of the experiment as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
ueDisplayName :: Lens' UpdateExperiment (Maybe Text)
ueDisplayName = lens _ueDisplayName (\s a -> s {_ueDisplayName = a})

-- | The description of the experiment.
ueDescription :: Lens' UpdateExperiment (Maybe Text)
ueDescription = lens _ueDescription (\s a -> s {_ueDescription = a})

-- | The name of the experiment to update.
ueExperimentName :: Lens' UpdateExperiment Text
ueExperimentName = lens _ueExperimentName (\s a -> s {_ueExperimentName = a})

instance AWSRequest UpdateExperiment where
  type Rs UpdateExperiment = UpdateExperimentResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          UpdateExperimentResponse'
            <$> (x .?> "ExperimentArn") <*> (pure (fromEnum s))
      )

instance Hashable UpdateExperiment

instance NFData UpdateExperiment

instance ToHeaders UpdateExperiment where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.UpdateExperiment" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateExperiment where
  toJSON UpdateExperiment' {..} =
    object
      ( catMaybes
          [ ("DisplayName" .=) <$> _ueDisplayName,
            ("Description" .=) <$> _ueDescription,
            Just ("ExperimentName" .= _ueExperimentName)
          ]
      )

instance ToPath UpdateExperiment where
  toPath = const "/"

instance ToQuery UpdateExperiment where
  toQuery = const mempty

-- | /See:/ 'updateExperimentResponse' smart constructor.
data UpdateExperimentResponse = UpdateExperimentResponse'
  { _ursExperimentARN ::
      !(Maybe Text),
    _ursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateExperimentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursExperimentARN' - The Amazon Resource Name (ARN) of the experiment.
--
-- * 'ursResponseStatus' - -- | The response status code.
updateExperimentResponse ::
  -- | 'ursResponseStatus'
  Int ->
  UpdateExperimentResponse
updateExperimentResponse pResponseStatus_ =
  UpdateExperimentResponse'
    { _ursExperimentARN = Nothing,
      _ursResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment.
ursExperimentARN :: Lens' UpdateExperimentResponse (Maybe Text)
ursExperimentARN = lens _ursExperimentARN (\s a -> s {_ursExperimentARN = a})

-- | -- | The response status code.
ursResponseStatus :: Lens' UpdateExperimentResponse Int
ursResponseStatus = lens _ursResponseStatus (\s a -> s {_ursResponseStatus = a})

instance NFData UpdateExperimentResponse
