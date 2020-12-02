{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageStatusDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ModelPackageStatusItem

-- | Specifies the validation and image scan statuses of the model package.
--
--
--
-- /See:/ 'modelPackageStatusDetails' smart constructor.
data ModelPackageStatusDetails = ModelPackageStatusDetails'
  { _mpsdImageScanStatuses ::
      !(Maybe [ModelPackageStatusItem]),
    _mpsdValidationStatuses ::
      ![ModelPackageStatusItem]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelPackageStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsdImageScanStatuses' - The status of the scan of the Docker image container for the model package.
--
-- * 'mpsdValidationStatuses' - The validation status of the model package.
modelPackageStatusDetails ::
  ModelPackageStatusDetails
modelPackageStatusDetails =
  ModelPackageStatusDetails'
    { _mpsdImageScanStatuses = Nothing,
      _mpsdValidationStatuses = mempty
    }

-- | The status of the scan of the Docker image container for the model package.
mpsdImageScanStatuses :: Lens' ModelPackageStatusDetails [ModelPackageStatusItem]
mpsdImageScanStatuses = lens _mpsdImageScanStatuses (\s a -> s {_mpsdImageScanStatuses = a}) . _Default . _Coerce

-- | The validation status of the model package.
mpsdValidationStatuses :: Lens' ModelPackageStatusDetails [ModelPackageStatusItem]
mpsdValidationStatuses = lens _mpsdValidationStatuses (\s a -> s {_mpsdValidationStatuses = a}) . _Coerce

instance FromJSON ModelPackageStatusDetails where
  parseJSON =
    withObject
      "ModelPackageStatusDetails"
      ( \x ->
          ModelPackageStatusDetails'
            <$> (x .:? "ImageScanStatuses" .!= mempty)
            <*> (x .:? "ValidationStatuses" .!= mempty)
      )

instance Hashable ModelPackageStatusDetails

instance NFData ModelPackageStatusDetails
