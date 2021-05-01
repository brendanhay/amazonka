{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData where

import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Attributes that define an Amazon SageMaker machine learning resource.
--
-- /See:/ 'newSageMakerMachineLearningModelResourceData' smart constructor.
data SageMakerMachineLearningModelResourceData = SageMakerMachineLearningModelResourceData'
  { ownerSetting :: Prelude.Maybe ResourceDownloadOwnerSetting,
    -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon SageMaker training job that represents the source
    -- model.
    sageMakerJobArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SageMakerMachineLearningModelResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerSetting', 'sageMakerMachineLearningModelResourceData_ownerSetting' - Undocumented member.
--
-- 'destinationPath', 'sageMakerMachineLearningModelResourceData_destinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- 'sageMakerJobArn', 'sageMakerMachineLearningModelResourceData_sageMakerJobArn' - The ARN of the Amazon SageMaker training job that represents the source
-- model.
newSageMakerMachineLearningModelResourceData ::
  SageMakerMachineLearningModelResourceData
newSageMakerMachineLearningModelResourceData =
  SageMakerMachineLearningModelResourceData'
    { ownerSetting =
        Prelude.Nothing,
      destinationPath =
        Prelude.Nothing,
      sageMakerJobArn =
        Prelude.Nothing
    }

-- | Undocumented member.
sageMakerMachineLearningModelResourceData_ownerSetting :: Lens.Lens' SageMakerMachineLearningModelResourceData (Prelude.Maybe ResourceDownloadOwnerSetting)
sageMakerMachineLearningModelResourceData_ownerSetting = Lens.lens (\SageMakerMachineLearningModelResourceData' {ownerSetting} -> ownerSetting) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {ownerSetting = a} :: SageMakerMachineLearningModelResourceData)

-- | The absolute local path of the resource inside the Lambda environment.
sageMakerMachineLearningModelResourceData_destinationPath :: Lens.Lens' SageMakerMachineLearningModelResourceData (Prelude.Maybe Prelude.Text)
sageMakerMachineLearningModelResourceData_destinationPath = Lens.lens (\SageMakerMachineLearningModelResourceData' {destinationPath} -> destinationPath) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {destinationPath = a} :: SageMakerMachineLearningModelResourceData)

-- | The ARN of the Amazon SageMaker training job that represents the source
-- model.
sageMakerMachineLearningModelResourceData_sageMakerJobArn :: Lens.Lens' SageMakerMachineLearningModelResourceData (Prelude.Maybe Prelude.Text)
sageMakerMachineLearningModelResourceData_sageMakerJobArn = Lens.lens (\SageMakerMachineLearningModelResourceData' {sageMakerJobArn} -> sageMakerJobArn) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {sageMakerJobArn = a} :: SageMakerMachineLearningModelResourceData)

instance
  Prelude.FromJSON
    SageMakerMachineLearningModelResourceData
  where
  parseJSON =
    Prelude.withObject
      "SageMakerMachineLearningModelResourceData"
      ( \x ->
          SageMakerMachineLearningModelResourceData'
            Prelude.<$> (x Prelude..:? "OwnerSetting")
              Prelude.<*> (x Prelude..:? "DestinationPath")
              Prelude.<*> (x Prelude..:? "SageMakerJobArn")
      )

instance
  Prelude.Hashable
    SageMakerMachineLearningModelResourceData

instance
  Prelude.NFData
    SageMakerMachineLearningModelResourceData

instance
  Prelude.ToJSON
    SageMakerMachineLearningModelResourceData
  where
  toJSON SageMakerMachineLearningModelResourceData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OwnerSetting" Prelude..=)
              Prelude.<$> ownerSetting,
            ("DestinationPath" Prelude..=)
              Prelude.<$> destinationPath,
            ("SageMakerJobArn" Prelude..=)
              Prelude.<$> sageMakerJobArn
          ]
      )
