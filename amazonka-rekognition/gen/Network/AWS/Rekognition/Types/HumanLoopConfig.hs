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
-- Module      : Network.AWS.Rekognition.Types.HumanLoopConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.HumanLoopDataAttributes

-- | Sets up the flow definition the image will be sent to if one of the
-- conditions is met. You can also set certain attributes of the image
-- before review.
--
-- /See:/ 'newHumanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { -- | Sets attributes of the input data.
    dataAttributes :: Prelude.Maybe HumanLoopDataAttributes,
    -- | The name of the human review used for this image. This should be kept
    -- unique within a region.
    humanLoopName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition. You can create a
    -- flow definition by using the Amazon Sagemaker
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition>
    -- Operation.
    flowDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAttributes', 'humanLoopConfig_dataAttributes' - Sets attributes of the input data.
--
-- 'humanLoopName', 'humanLoopConfig_humanLoopName' - The name of the human review used for this image. This should be kept
-- unique within a region.
--
-- 'flowDefinitionArn', 'humanLoopConfig_flowDefinitionArn' - The Amazon Resource Name (ARN) of the flow definition. You can create a
-- flow definition by using the Amazon Sagemaker
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition>
-- Operation.
newHumanLoopConfig ::
  -- | 'humanLoopName'
  Prelude.Text ->
  -- | 'flowDefinitionArn'
  Prelude.Text ->
  HumanLoopConfig
newHumanLoopConfig
  pHumanLoopName_
  pFlowDefinitionArn_ =
    HumanLoopConfig'
      { dataAttributes = Prelude.Nothing,
        humanLoopName = pHumanLoopName_,
        flowDefinitionArn = pFlowDefinitionArn_
      }

-- | Sets attributes of the input data.
humanLoopConfig_dataAttributes :: Lens.Lens' HumanLoopConfig (Prelude.Maybe HumanLoopDataAttributes)
humanLoopConfig_dataAttributes = Lens.lens (\HumanLoopConfig' {dataAttributes} -> dataAttributes) (\s@HumanLoopConfig' {} a -> s {dataAttributes = a} :: HumanLoopConfig)

-- | The name of the human review used for this image. This should be kept
-- unique within a region.
humanLoopConfig_humanLoopName :: Lens.Lens' HumanLoopConfig Prelude.Text
humanLoopConfig_humanLoopName = Lens.lens (\HumanLoopConfig' {humanLoopName} -> humanLoopName) (\s@HumanLoopConfig' {} a -> s {humanLoopName = a} :: HumanLoopConfig)

-- | The Amazon Resource Name (ARN) of the flow definition. You can create a
-- flow definition by using the Amazon Sagemaker
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition>
-- Operation.
humanLoopConfig_flowDefinitionArn :: Lens.Lens' HumanLoopConfig Prelude.Text
humanLoopConfig_flowDefinitionArn = Lens.lens (\HumanLoopConfig' {flowDefinitionArn} -> flowDefinitionArn) (\s@HumanLoopConfig' {} a -> s {flowDefinitionArn = a} :: HumanLoopConfig)

instance Prelude.Hashable HumanLoopConfig

instance Prelude.NFData HumanLoopConfig

instance Prelude.ToJSON HumanLoopConfig where
  toJSON HumanLoopConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DataAttributes" Prelude..=)
              Prelude.<$> dataAttributes,
            Prelude.Just
              ("HumanLoopName" Prelude..= humanLoopName),
            Prelude.Just
              ("FlowDefinitionArn" Prelude..= flowDefinitionArn)
          ]
      )
