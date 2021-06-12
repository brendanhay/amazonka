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

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import qualified Network.AWS.Lens as Lens

-- | Attributes that define an Amazon SageMaker machine learning resource.
--
-- /See:/ 'newSageMakerMachineLearningModelResourceData' smart constructor.
data SageMakerMachineLearningModelResourceData = SageMakerMachineLearningModelResourceData'
  { ownerSetting :: Core.Maybe ResourceDownloadOwnerSetting,
    -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Core.Maybe Core.Text,
    -- | The ARN of the Amazon SageMaker training job that represents the source
    -- model.
    sageMakerJobArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      destinationPath = Core.Nothing,
      sageMakerJobArn = Core.Nothing
    }

-- | Undocumented member.
sageMakerMachineLearningModelResourceData_ownerSetting :: Lens.Lens' SageMakerMachineLearningModelResourceData (Core.Maybe ResourceDownloadOwnerSetting)
sageMakerMachineLearningModelResourceData_ownerSetting = Lens.lens (\SageMakerMachineLearningModelResourceData' {ownerSetting} -> ownerSetting) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {ownerSetting = a} :: SageMakerMachineLearningModelResourceData)

-- | The absolute local path of the resource inside the Lambda environment.
sageMakerMachineLearningModelResourceData_destinationPath :: Lens.Lens' SageMakerMachineLearningModelResourceData (Core.Maybe Core.Text)
sageMakerMachineLearningModelResourceData_destinationPath = Lens.lens (\SageMakerMachineLearningModelResourceData' {destinationPath} -> destinationPath) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {destinationPath = a} :: SageMakerMachineLearningModelResourceData)

-- | The ARN of the Amazon SageMaker training job that represents the source
-- model.
sageMakerMachineLearningModelResourceData_sageMakerJobArn :: Lens.Lens' SageMakerMachineLearningModelResourceData (Core.Maybe Core.Text)
sageMakerMachineLearningModelResourceData_sageMakerJobArn = Lens.lens (\SageMakerMachineLearningModelResourceData' {sageMakerJobArn} -> sageMakerJobArn) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {sageMakerJobArn = a} :: SageMakerMachineLearningModelResourceData)

instance
  Core.FromJSON
    SageMakerMachineLearningModelResourceData
  where
  parseJSON =
    Core.withObject
      "SageMakerMachineLearningModelResourceData"
      ( \x ->
          SageMakerMachineLearningModelResourceData'
            Core.<$> (x Core..:? "OwnerSetting")
            Core.<*> (x Core..:? "DestinationPath")
            Core.<*> (x Core..:? "SageMakerJobArn")
      )

instance
  Core.Hashable
    SageMakerMachineLearningModelResourceData

instance
  Core.NFData
    SageMakerMachineLearningModelResourceData

instance
  Core.ToJSON
    SageMakerMachineLearningModelResourceData
  where
  toJSON SageMakerMachineLearningModelResourceData' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OwnerSetting" Core..=) Core.<$> ownerSetting,
            ("DestinationPath" Core..=) Core.<$> destinationPath,
            ("SageMakerJobArn" Core..=)
              Core.<$> sageMakerJobArn
          ]
      )
