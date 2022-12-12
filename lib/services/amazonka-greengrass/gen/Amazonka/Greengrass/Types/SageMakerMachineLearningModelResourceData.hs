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
-- Module      : Amazonka.Greengrass.Types.SageMakerMachineLearningModelResourceData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.SageMakerMachineLearningModelResourceData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.ResourceDownloadOwnerSetting
import qualified Amazonka.Prelude as Prelude

-- | Attributes that define an Amazon SageMaker machine learning resource.
--
-- /See:/ 'newSageMakerMachineLearningModelResourceData' smart constructor.
data SageMakerMachineLearningModelResourceData = SageMakerMachineLearningModelResourceData'
  { -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Prelude.Maybe Prelude.Text,
    ownerSetting :: Prelude.Maybe ResourceDownloadOwnerSetting,
    -- | The ARN of the Amazon SageMaker training job that represents the source
    -- model.
    sageMakerJobArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SageMakerMachineLearningModelResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPath', 'sageMakerMachineLearningModelResourceData_destinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- 'ownerSetting', 'sageMakerMachineLearningModelResourceData_ownerSetting' - Undocumented member.
--
-- 'sageMakerJobArn', 'sageMakerMachineLearningModelResourceData_sageMakerJobArn' - The ARN of the Amazon SageMaker training job that represents the source
-- model.
newSageMakerMachineLearningModelResourceData ::
  SageMakerMachineLearningModelResourceData
newSageMakerMachineLearningModelResourceData =
  SageMakerMachineLearningModelResourceData'
    { destinationPath =
        Prelude.Nothing,
      ownerSetting = Prelude.Nothing,
      sageMakerJobArn =
        Prelude.Nothing
    }

-- | The absolute local path of the resource inside the Lambda environment.
sageMakerMachineLearningModelResourceData_destinationPath :: Lens.Lens' SageMakerMachineLearningModelResourceData (Prelude.Maybe Prelude.Text)
sageMakerMachineLearningModelResourceData_destinationPath = Lens.lens (\SageMakerMachineLearningModelResourceData' {destinationPath} -> destinationPath) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {destinationPath = a} :: SageMakerMachineLearningModelResourceData)

-- | Undocumented member.
sageMakerMachineLearningModelResourceData_ownerSetting :: Lens.Lens' SageMakerMachineLearningModelResourceData (Prelude.Maybe ResourceDownloadOwnerSetting)
sageMakerMachineLearningModelResourceData_ownerSetting = Lens.lens (\SageMakerMachineLearningModelResourceData' {ownerSetting} -> ownerSetting) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {ownerSetting = a} :: SageMakerMachineLearningModelResourceData)

-- | The ARN of the Amazon SageMaker training job that represents the source
-- model.
sageMakerMachineLearningModelResourceData_sageMakerJobArn :: Lens.Lens' SageMakerMachineLearningModelResourceData (Prelude.Maybe Prelude.Text)
sageMakerMachineLearningModelResourceData_sageMakerJobArn = Lens.lens (\SageMakerMachineLearningModelResourceData' {sageMakerJobArn} -> sageMakerJobArn) (\s@SageMakerMachineLearningModelResourceData' {} a -> s {sageMakerJobArn = a} :: SageMakerMachineLearningModelResourceData)

instance
  Data.FromJSON
    SageMakerMachineLearningModelResourceData
  where
  parseJSON =
    Data.withObject
      "SageMakerMachineLearningModelResourceData"
      ( \x ->
          SageMakerMachineLearningModelResourceData'
            Prelude.<$> (x Data..:? "DestinationPath")
              Prelude.<*> (x Data..:? "OwnerSetting")
              Prelude.<*> (x Data..:? "SageMakerJobArn")
      )

instance
  Prelude.Hashable
    SageMakerMachineLearningModelResourceData
  where
  hashWithSalt
    _salt
    SageMakerMachineLearningModelResourceData' {..} =
      _salt `Prelude.hashWithSalt` destinationPath
        `Prelude.hashWithSalt` ownerSetting
        `Prelude.hashWithSalt` sageMakerJobArn

instance
  Prelude.NFData
    SageMakerMachineLearningModelResourceData
  where
  rnf SageMakerMachineLearningModelResourceData' {..} =
    Prelude.rnf destinationPath
      `Prelude.seq` Prelude.rnf ownerSetting
      `Prelude.seq` Prelude.rnf sageMakerJobArn

instance
  Data.ToJSON
    SageMakerMachineLearningModelResourceData
  where
  toJSON SageMakerMachineLearningModelResourceData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationPath" Data..=)
              Prelude.<$> destinationPath,
            ("OwnerSetting" Data..=) Prelude.<$> ownerSetting,
            ("SageMakerJobArn" Data..=)
              Prelude.<$> sageMakerJobArn
          ]
      )
