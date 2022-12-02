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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ProcessorConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ProcessorConfiguration where

import Amazonka.ChimeSDKMessaging.Types.LambdaConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A processor\'s metadata.
--
-- /See:/ 'newProcessorConfiguration' smart constructor.
data ProcessorConfiguration = ProcessorConfiguration'
  { -- | Indicates that the processor is of type Lambda.
    lambda :: LambdaConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambda', 'processorConfiguration_lambda' - Indicates that the processor is of type Lambda.
newProcessorConfiguration ::
  -- | 'lambda'
  LambdaConfiguration ->
  ProcessorConfiguration
newProcessorConfiguration pLambda_ =
  ProcessorConfiguration' {lambda = pLambda_}

-- | Indicates that the processor is of type Lambda.
processorConfiguration_lambda :: Lens.Lens' ProcessorConfiguration LambdaConfiguration
processorConfiguration_lambda = Lens.lens (\ProcessorConfiguration' {lambda} -> lambda) (\s@ProcessorConfiguration' {} a -> s {lambda = a} :: ProcessorConfiguration)

instance Data.FromJSON ProcessorConfiguration where
  parseJSON =
    Data.withObject
      "ProcessorConfiguration"
      ( \x ->
          ProcessorConfiguration'
            Prelude.<$> (x Data..: "Lambda")
      )

instance Prelude.Hashable ProcessorConfiguration where
  hashWithSalt _salt ProcessorConfiguration' {..} =
    _salt `Prelude.hashWithSalt` lambda

instance Prelude.NFData ProcessorConfiguration where
  rnf ProcessorConfiguration' {..} = Prelude.rnf lambda

instance Data.ToJSON ProcessorConfiguration where
  toJSON ProcessorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Lambda" Data..= lambda)]
      )
