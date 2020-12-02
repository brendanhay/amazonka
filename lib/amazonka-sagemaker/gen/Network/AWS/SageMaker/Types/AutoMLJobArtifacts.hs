{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobArtifacts where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Artifacts that are generation during a job.
--
--
--
-- /See:/ 'autoMLJobArtifacts' smart constructor.
data AutoMLJobArtifacts = AutoMLJobArtifacts'
  { _amljaCandidateDefinitionNotebookLocation ::
      !(Maybe Text),
    _amljaDataExplorationNotebookLocation ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLJobArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amljaCandidateDefinitionNotebookLocation' - The URL to the notebook location.
--
-- * 'amljaDataExplorationNotebookLocation' - The URL to the notebook location.
autoMLJobArtifacts ::
  AutoMLJobArtifacts
autoMLJobArtifacts =
  AutoMLJobArtifacts'
    { _amljaCandidateDefinitionNotebookLocation =
        Nothing,
      _amljaDataExplorationNotebookLocation = Nothing
    }

-- | The URL to the notebook location.
amljaCandidateDefinitionNotebookLocation :: Lens' AutoMLJobArtifacts (Maybe Text)
amljaCandidateDefinitionNotebookLocation = lens _amljaCandidateDefinitionNotebookLocation (\s a -> s {_amljaCandidateDefinitionNotebookLocation = a})

-- | The URL to the notebook location.
amljaDataExplorationNotebookLocation :: Lens' AutoMLJobArtifacts (Maybe Text)
amljaDataExplorationNotebookLocation = lens _amljaDataExplorationNotebookLocation (\s a -> s {_amljaDataExplorationNotebookLocation = a})

instance FromJSON AutoMLJobArtifacts where
  parseJSON =
    withObject
      "AutoMLJobArtifacts"
      ( \x ->
          AutoMLJobArtifacts'
            <$> (x .:? "CandidateDefinitionNotebookLocation")
            <*> (x .:? "DataExplorationNotebookLocation")
      )

instance Hashable AutoMLJobArtifacts

instance NFData AutoMLJobArtifacts
