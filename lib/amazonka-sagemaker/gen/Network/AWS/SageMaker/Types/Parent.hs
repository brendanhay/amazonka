{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Parent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Parent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The trial that a trial component is associated with and the experiment the trial is part of. A component might not be associated with a trial. A component can be associated with multiple trials.
--
--
--
-- /See:/ 'parent' smart constructor.
data Parent = Parent'
  { _pExperimentName :: !(Maybe Text),
    _pTrialName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Parent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pExperimentName' - The name of the experiment.
--
-- * 'pTrialName' - The name of the trial.
parent ::
  Parent
parent = Parent' {_pExperimentName = Nothing, _pTrialName = Nothing}

-- | The name of the experiment.
pExperimentName :: Lens' Parent (Maybe Text)
pExperimentName = lens _pExperimentName (\s a -> s {_pExperimentName = a})

-- | The name of the trial.
pTrialName :: Lens' Parent (Maybe Text)
pTrialName = lens _pTrialName (\s a -> s {_pTrialName = a})

instance FromJSON Parent where
  parseJSON =
    withObject
      "Parent"
      ( \x ->
          Parent' <$> (x .:? "ExperimentName") <*> (x .:? "TrialName")
      )

instance Hashable Parent

instance NFData Parent
