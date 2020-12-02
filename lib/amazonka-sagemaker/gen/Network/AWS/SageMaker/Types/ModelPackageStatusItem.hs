{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageStatusItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.DetailedModelPackageStatus

-- | Represents the overall status of a model package.
--
--
--
-- /See:/ 'modelPackageStatusItem' smart constructor.
data ModelPackageStatusItem = ModelPackageStatusItem'
  { _mpsiFailureReason ::
      !(Maybe Text),
    _mpsiName :: !Text,
    _mpsiStatus :: !DetailedModelPackageStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelPackageStatusItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsiFailureReason' - if the overall status is @Failed@ , the reason for the failure.
--
-- * 'mpsiName' - The name of the model package for which the overall status is being reported.
--
-- * 'mpsiStatus' - The current status.
modelPackageStatusItem ::
  -- | 'mpsiName'
  Text ->
  -- | 'mpsiStatus'
  DetailedModelPackageStatus ->
  ModelPackageStatusItem
modelPackageStatusItem pName_ pStatus_ =
  ModelPackageStatusItem'
    { _mpsiFailureReason = Nothing,
      _mpsiName = pName_,
      _mpsiStatus = pStatus_
    }

-- | if the overall status is @Failed@ , the reason for the failure.
mpsiFailureReason :: Lens' ModelPackageStatusItem (Maybe Text)
mpsiFailureReason = lens _mpsiFailureReason (\s a -> s {_mpsiFailureReason = a})

-- | The name of the model package for which the overall status is being reported.
mpsiName :: Lens' ModelPackageStatusItem Text
mpsiName = lens _mpsiName (\s a -> s {_mpsiName = a})

-- | The current status.
mpsiStatus :: Lens' ModelPackageStatusItem DetailedModelPackageStatus
mpsiStatus = lens _mpsiStatus (\s a -> s {_mpsiStatus = a})

instance FromJSON ModelPackageStatusItem where
  parseJSON =
    withObject
      "ModelPackageStatusItem"
      ( \x ->
          ModelPackageStatusItem'
            <$> (x .:? "FailureReason") <*> (x .: "Name") <*> (x .: "Status")
      )

instance Hashable ModelPackageStatusItem

instance NFData ModelPackageStatusItem
