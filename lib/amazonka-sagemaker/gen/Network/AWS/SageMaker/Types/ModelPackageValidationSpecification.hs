{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageValidationSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ModelPackageValidationProfile

-- | Specifies batch transform jobs that Amazon SageMaker runs to validate your model package.
--
--
--
-- /See:/ 'modelPackageValidationSpecification' smart constructor.
data ModelPackageValidationSpecification = ModelPackageValidationSpecification'
  { _mpvsValidationRole ::
      !Text,
    _mpvsValidationProfiles ::
      !( List1
           ModelPackageValidationProfile
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelPackageValidationSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpvsValidationRole' - The IAM roles to be used for the validation of the model package.
--
-- * 'mpvsValidationProfiles' - An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
modelPackageValidationSpecification ::
  -- | 'mpvsValidationRole'
  Text ->
  -- | 'mpvsValidationProfiles'
  NonEmpty ModelPackageValidationProfile ->
  ModelPackageValidationSpecification
modelPackageValidationSpecification
  pValidationRole_
  pValidationProfiles_ =
    ModelPackageValidationSpecification'
      { _mpvsValidationRole =
          pValidationRole_,
        _mpvsValidationProfiles = _List1 # pValidationProfiles_
      }

-- | The IAM roles to be used for the validation of the model package.
mpvsValidationRole :: Lens' ModelPackageValidationSpecification Text
mpvsValidationRole = lens _mpvsValidationRole (\s a -> s {_mpvsValidationRole = a})

-- | An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
mpvsValidationProfiles :: Lens' ModelPackageValidationSpecification (NonEmpty ModelPackageValidationProfile)
mpvsValidationProfiles = lens _mpvsValidationProfiles (\s a -> s {_mpvsValidationProfiles = a}) . _List1

instance FromJSON ModelPackageValidationSpecification where
  parseJSON =
    withObject
      "ModelPackageValidationSpecification"
      ( \x ->
          ModelPackageValidationSpecification'
            <$> (x .: "ValidationRole") <*> (x .: "ValidationProfiles")
      )

instance Hashable ModelPackageValidationSpecification

instance NFData ModelPackageValidationSpecification

instance ToJSON ModelPackageValidationSpecification where
  toJSON ModelPackageValidationSpecification' {..} =
    object
      ( catMaybes
          [ Just ("ValidationRole" .= _mpvsValidationRole),
            Just ("ValidationProfiles" .= _mpvsValidationProfiles)
          ]
      )
