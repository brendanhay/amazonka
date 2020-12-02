{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmValidationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmValidationSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AlgorithmValidationProfile

-- | Specifies configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
--
--
-- /See:/ 'algorithmValidationSpecification' smart constructor.
data AlgorithmValidationSpecification = AlgorithmValidationSpecification'
  { _avsValidationRole ::
      !Text,
    _avsValidationProfiles ::
      !( List1
           AlgorithmValidationProfile
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlgorithmValidationSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avsValidationRole' - The IAM roles that Amazon SageMaker uses to run the training jobs.
--
-- * 'avsValidationProfiles' - An array of @AlgorithmValidationProfile@ objects, each of which specifies a training job and batch transform job that Amazon SageMaker runs to validate your algorithm.
algorithmValidationSpecification ::
  -- | 'avsValidationRole'
  Text ->
  -- | 'avsValidationProfiles'
  NonEmpty AlgorithmValidationProfile ->
  AlgorithmValidationSpecification
algorithmValidationSpecification
  pValidationRole_
  pValidationProfiles_ =
    AlgorithmValidationSpecification'
      { _avsValidationRole =
          pValidationRole_,
        _avsValidationProfiles = _List1 # pValidationProfiles_
      }

-- | The IAM roles that Amazon SageMaker uses to run the training jobs.
avsValidationRole :: Lens' AlgorithmValidationSpecification Text
avsValidationRole = lens _avsValidationRole (\s a -> s {_avsValidationRole = a})

-- | An array of @AlgorithmValidationProfile@ objects, each of which specifies a training job and batch transform job that Amazon SageMaker runs to validate your algorithm.
avsValidationProfiles :: Lens' AlgorithmValidationSpecification (NonEmpty AlgorithmValidationProfile)
avsValidationProfiles = lens _avsValidationProfiles (\s a -> s {_avsValidationProfiles = a}) . _List1

instance FromJSON AlgorithmValidationSpecification where
  parseJSON =
    withObject
      "AlgorithmValidationSpecification"
      ( \x ->
          AlgorithmValidationSpecification'
            <$> (x .: "ValidationRole") <*> (x .: "ValidationProfiles")
      )

instance Hashable AlgorithmValidationSpecification

instance NFData AlgorithmValidationSpecification

instance ToJSON AlgorithmValidationSpecification where
  toJSON AlgorithmValidationSpecification' {..} =
    object
      ( catMaybes
          [ Just ("ValidationRole" .= _avsValidationRole),
            Just ("ValidationProfiles" .= _avsValidationProfiles)
          ]
      )
