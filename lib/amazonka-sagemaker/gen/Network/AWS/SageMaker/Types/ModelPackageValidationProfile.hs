{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageValidationProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageValidationProfile where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TransformJobDefinition

-- | Contains data, such as the inputs and targeted instance types that are used in the process of validating the model package.
--
--
-- The data provided in the validation profile is made available to your buyers on AWS Marketplace.
--
--
-- /See:/ 'modelPackageValidationProfile' smart constructor.
data ModelPackageValidationProfile = ModelPackageValidationProfile'
  { _mpvpProfileName ::
      !Text,
    _mpvpTransformJobDefinition ::
      !TransformJobDefinition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelPackageValidationProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpvpProfileName' - The name of the profile for the model package.
--
-- * 'mpvpTransformJobDefinition' - The @TransformJobDefinition@ object that describes the transform job used for the validation of the model package.
modelPackageValidationProfile ::
  -- | 'mpvpProfileName'
  Text ->
  -- | 'mpvpTransformJobDefinition'
  TransformJobDefinition ->
  ModelPackageValidationProfile
modelPackageValidationProfile
  pProfileName_
  pTransformJobDefinition_ =
    ModelPackageValidationProfile'
      { _mpvpProfileName = pProfileName_,
        _mpvpTransformJobDefinition = pTransformJobDefinition_
      }

-- | The name of the profile for the model package.
mpvpProfileName :: Lens' ModelPackageValidationProfile Text
mpvpProfileName = lens _mpvpProfileName (\s a -> s {_mpvpProfileName = a})

-- | The @TransformJobDefinition@ object that describes the transform job used for the validation of the model package.
mpvpTransformJobDefinition :: Lens' ModelPackageValidationProfile TransformJobDefinition
mpvpTransformJobDefinition = lens _mpvpTransformJobDefinition (\s a -> s {_mpvpTransformJobDefinition = a})

instance FromJSON ModelPackageValidationProfile where
  parseJSON =
    withObject
      "ModelPackageValidationProfile"
      ( \x ->
          ModelPackageValidationProfile'
            <$> (x .: "ProfileName") <*> (x .: "TransformJobDefinition")
      )

instance Hashable ModelPackageValidationProfile

instance NFData ModelPackageValidationProfile

instance ToJSON ModelPackageValidationProfile where
  toJSON ModelPackageValidationProfile' {..} =
    object
      ( catMaybes
          [ Just ("ProfileName" .= _mpvpProfileName),
            Just ("TransformJobDefinition" .= _mpvpTransformJobDefinition)
          ]
      )
