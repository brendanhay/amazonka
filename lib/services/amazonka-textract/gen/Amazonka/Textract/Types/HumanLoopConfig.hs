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
-- Module      : Amazonka.Textract.Types.HumanLoopConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.HumanLoopConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.HumanLoopDataAttributes

-- | Sets up the human review workflow the document will be sent to if one of
-- the conditions is met. You can also set certain attributes of the image
-- before review.
--
-- /See:/ 'newHumanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { -- | Sets attributes of the input data.
    dataAttributes :: Prelude.Maybe HumanLoopDataAttributes,
    -- | The name of the human workflow used for this image. This should be kept
    -- unique within a region.
    humanLoopName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'humanLoopName', 'humanLoopConfig_humanLoopName' - The name of the human workflow used for this image. This should be kept
-- unique within a region.
--
-- 'flowDefinitionArn', 'humanLoopConfig_flowDefinitionArn' - The Amazon Resource Name (ARN) of the flow definition.
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

-- | The name of the human workflow used for this image. This should be kept
-- unique within a region.
humanLoopConfig_humanLoopName :: Lens.Lens' HumanLoopConfig Prelude.Text
humanLoopConfig_humanLoopName = Lens.lens (\HumanLoopConfig' {humanLoopName} -> humanLoopName) (\s@HumanLoopConfig' {} a -> s {humanLoopName = a} :: HumanLoopConfig)

-- | The Amazon Resource Name (ARN) of the flow definition.
humanLoopConfig_flowDefinitionArn :: Lens.Lens' HumanLoopConfig Prelude.Text
humanLoopConfig_flowDefinitionArn = Lens.lens (\HumanLoopConfig' {flowDefinitionArn} -> flowDefinitionArn) (\s@HumanLoopConfig' {} a -> s {flowDefinitionArn = a} :: HumanLoopConfig)

instance Prelude.Hashable HumanLoopConfig where
  hashWithSalt _salt HumanLoopConfig' {..} =
    _salt `Prelude.hashWithSalt` dataAttributes
      `Prelude.hashWithSalt` humanLoopName
      `Prelude.hashWithSalt` flowDefinitionArn

instance Prelude.NFData HumanLoopConfig where
  rnf HumanLoopConfig' {..} =
    Prelude.rnf dataAttributes
      `Prelude.seq` Prelude.rnf humanLoopName
      `Prelude.seq` Prelude.rnf flowDefinitionArn

instance Data.ToJSON HumanLoopConfig where
  toJSON HumanLoopConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataAttributes" Data..=)
              Prelude.<$> dataAttributes,
            Prelude.Just ("HumanLoopName" Data..= humanLoopName),
            Prelude.Just
              ("FlowDefinitionArn" Data..= flowDefinitionArn)
          ]
      )
