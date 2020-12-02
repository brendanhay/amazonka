{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A previously completed or stopped hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
--
--
--
-- /See:/ 'parentHyperParameterTuningJob' smart constructor.
newtype ParentHyperParameterTuningJob = ParentHyperParameterTuningJob'
  { _phptjHyperParameterTuningJobName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParentHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phptjHyperParameterTuningJobName' - The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
parentHyperParameterTuningJob ::
  ParentHyperParameterTuningJob
parentHyperParameterTuningJob =
  ParentHyperParameterTuningJob'
    { _phptjHyperParameterTuningJobName =
        Nothing
    }

-- | The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
phptjHyperParameterTuningJobName :: Lens' ParentHyperParameterTuningJob (Maybe Text)
phptjHyperParameterTuningJobName = lens _phptjHyperParameterTuningJobName (\s a -> s {_phptjHyperParameterTuningJobName = a})

instance FromJSON ParentHyperParameterTuningJob where
  parseJSON =
    withObject
      "ParentHyperParameterTuningJob"
      ( \x ->
          ParentHyperParameterTuningJob'
            <$> (x .:? "HyperParameterTuningJobName")
      )

instance Hashable ParentHyperParameterTuningJob

instance NFData ParentHyperParameterTuningJob

instance ToJSON ParentHyperParameterTuningJob where
  toJSON ParentHyperParameterTuningJob' {..} =
    object
      ( catMaybes
          [ ("HyperParameterTuningJobName" .=)
              <$> _phptjHyperParameterTuningJobName
          ]
      )
