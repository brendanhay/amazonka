{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ModelPackageStatus

-- | Provides summary information about a model package.
--
--
--
-- /See:/ 'modelPackageSummary' smart constructor.
data ModelPackageSummary = ModelPackageSummary'
  { _mpsModelPackageDescription ::
      !(Maybe Text),
    _mpsModelPackageName :: !Text,
    _mpsModelPackageARN :: !Text,
    _mpsCreationTime :: !POSIX,
    _mpsModelPackageStatus :: !ModelPackageStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelPackageSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsModelPackageDescription' - A brief description of the model package.
--
-- * 'mpsModelPackageName' - The name of the model package.
--
-- * 'mpsModelPackageARN' - The Amazon Resource Name (ARN) of the model package.
--
-- * 'mpsCreationTime' - A timestamp that shows when the model package was created.
--
-- * 'mpsModelPackageStatus' - The overall status of the model package.
modelPackageSummary ::
  -- | 'mpsModelPackageName'
  Text ->
  -- | 'mpsModelPackageARN'
  Text ->
  -- | 'mpsCreationTime'
  UTCTime ->
  -- | 'mpsModelPackageStatus'
  ModelPackageStatus ->
  ModelPackageSummary
modelPackageSummary
  pModelPackageName_
  pModelPackageARN_
  pCreationTime_
  pModelPackageStatus_ =
    ModelPackageSummary'
      { _mpsModelPackageDescription = Nothing,
        _mpsModelPackageName = pModelPackageName_,
        _mpsModelPackageARN = pModelPackageARN_,
        _mpsCreationTime = _Time # pCreationTime_,
        _mpsModelPackageStatus = pModelPackageStatus_
      }

-- | A brief description of the model package.
mpsModelPackageDescription :: Lens' ModelPackageSummary (Maybe Text)
mpsModelPackageDescription = lens _mpsModelPackageDescription (\s a -> s {_mpsModelPackageDescription = a})

-- | The name of the model package.
mpsModelPackageName :: Lens' ModelPackageSummary Text
mpsModelPackageName = lens _mpsModelPackageName (\s a -> s {_mpsModelPackageName = a})

-- | The Amazon Resource Name (ARN) of the model package.
mpsModelPackageARN :: Lens' ModelPackageSummary Text
mpsModelPackageARN = lens _mpsModelPackageARN (\s a -> s {_mpsModelPackageARN = a})

-- | A timestamp that shows when the model package was created.
mpsCreationTime :: Lens' ModelPackageSummary UTCTime
mpsCreationTime = lens _mpsCreationTime (\s a -> s {_mpsCreationTime = a}) . _Time

-- | The overall status of the model package.
mpsModelPackageStatus :: Lens' ModelPackageSummary ModelPackageStatus
mpsModelPackageStatus = lens _mpsModelPackageStatus (\s a -> s {_mpsModelPackageStatus = a})

instance FromJSON ModelPackageSummary where
  parseJSON =
    withObject
      "ModelPackageSummary"
      ( \x ->
          ModelPackageSummary'
            <$> (x .:? "ModelPackageDescription")
            <*> (x .: "ModelPackageName")
            <*> (x .: "ModelPackageArn")
            <*> (x .: "CreationTime")
            <*> (x .: "ModelPackageStatus")
      )

instance Hashable ModelPackageSummary

instance NFData ModelPackageSummary
