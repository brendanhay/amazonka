{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidateStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLCandidateStep where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CandidateStepType

-- | Information about the steps for a Candidate, and what step it is working on.
--
--
--
-- /See:/ 'autoMLCandidateStep' smart constructor.
data AutoMLCandidateStep = AutoMLCandidateStep'
  { _amlcsCandidateStepType ::
      !CandidateStepType,
    _amlcsCandidateStepARN :: !Text,
    _amlcsCandidateStepName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLCandidateStep' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlcsCandidateStepType' - Whether the Candidate is at the transform, training, or processing step.
--
-- * 'amlcsCandidateStepARN' - The ARN for the Candidate's step.
--
-- * 'amlcsCandidateStepName' - The name for the Candidate's step.
autoMLCandidateStep ::
  -- | 'amlcsCandidateStepType'
  CandidateStepType ->
  -- | 'amlcsCandidateStepARN'
  Text ->
  -- | 'amlcsCandidateStepName'
  Text ->
  AutoMLCandidateStep
autoMLCandidateStep
  pCandidateStepType_
  pCandidateStepARN_
  pCandidateStepName_ =
    AutoMLCandidateStep'
      { _amlcsCandidateStepType =
          pCandidateStepType_,
        _amlcsCandidateStepARN = pCandidateStepARN_,
        _amlcsCandidateStepName = pCandidateStepName_
      }

-- | Whether the Candidate is at the transform, training, or processing step.
amlcsCandidateStepType :: Lens' AutoMLCandidateStep CandidateStepType
amlcsCandidateStepType = lens _amlcsCandidateStepType (\s a -> s {_amlcsCandidateStepType = a})

-- | The ARN for the Candidate's step.
amlcsCandidateStepARN :: Lens' AutoMLCandidateStep Text
amlcsCandidateStepARN = lens _amlcsCandidateStepARN (\s a -> s {_amlcsCandidateStepARN = a})

-- | The name for the Candidate's step.
amlcsCandidateStepName :: Lens' AutoMLCandidateStep Text
amlcsCandidateStepName = lens _amlcsCandidateStepName (\s a -> s {_amlcsCandidateStepName = a})

instance FromJSON AutoMLCandidateStep where
  parseJSON =
    withObject
      "AutoMLCandidateStep"
      ( \x ->
          AutoMLCandidateStep'
            <$> (x .: "CandidateStepType")
            <*> (x .: "CandidateStepArn")
            <*> (x .: "CandidateStepName")
      )

instance Hashable AutoMLCandidateStep

instance NFData AutoMLCandidateStep
