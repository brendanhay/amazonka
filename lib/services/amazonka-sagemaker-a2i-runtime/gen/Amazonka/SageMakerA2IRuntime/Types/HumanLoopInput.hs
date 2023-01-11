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
-- Module      : Amazonka.SageMakerA2IRuntime.Types.HumanLoopInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerA2IRuntime.Types.HumanLoopInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object containing the human loop input in JSON format.
--
-- /See:/ 'newHumanLoopInput' smart constructor.
data HumanLoopInput = HumanLoopInput'
  { -- | Serialized input from the human loop. The input must be a string
    -- representation of a file in JSON format.
    inputContent :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputContent', 'humanLoopInput_inputContent' - Serialized input from the human loop. The input must be a string
-- representation of a file in JSON format.
newHumanLoopInput ::
  -- | 'inputContent'
  Prelude.Text ->
  HumanLoopInput
newHumanLoopInput pInputContent_ =
  HumanLoopInput' {inputContent = pInputContent_}

-- | Serialized input from the human loop. The input must be a string
-- representation of a file in JSON format.
humanLoopInput_inputContent :: Lens.Lens' HumanLoopInput Prelude.Text
humanLoopInput_inputContent = Lens.lens (\HumanLoopInput' {inputContent} -> inputContent) (\s@HumanLoopInput' {} a -> s {inputContent = a} :: HumanLoopInput)

instance Prelude.Hashable HumanLoopInput where
  hashWithSalt _salt HumanLoopInput' {..} =
    _salt `Prelude.hashWithSalt` inputContent

instance Prelude.NFData HumanLoopInput where
  rnf HumanLoopInput' {..} = Prelude.rnf inputContent

instance Data.ToJSON HumanLoopInput where
  toJSON HumanLoopInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("InputContent" Data..= inputContent)]
      )
