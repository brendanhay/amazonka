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
-- Module      : Amazonka.Synthetics.Types.CanaryCodeOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryCodeOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains information about the canary\'s Lambda handler
-- and where its code is stored by CloudWatch Synthetics.
--
-- /See:/ 'newCanaryCodeOutput' smart constructor.
data CanaryCodeOutput = CanaryCodeOutput'
  { -- | The ARN of the Lambda layer where Synthetics stores the canary script
    -- code.
    sourceLocationArn :: Prelude.Maybe Prelude.Text,
    -- | The entry point to use for the source code when running the canary.
    handler :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryCodeOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceLocationArn', 'canaryCodeOutput_sourceLocationArn' - The ARN of the Lambda layer where Synthetics stores the canary script
-- code.
--
-- 'handler', 'canaryCodeOutput_handler' - The entry point to use for the source code when running the canary.
newCanaryCodeOutput ::
  CanaryCodeOutput
newCanaryCodeOutput =
  CanaryCodeOutput'
    { sourceLocationArn =
        Prelude.Nothing,
      handler = Prelude.Nothing
    }

-- | The ARN of the Lambda layer where Synthetics stores the canary script
-- code.
canaryCodeOutput_sourceLocationArn :: Lens.Lens' CanaryCodeOutput (Prelude.Maybe Prelude.Text)
canaryCodeOutput_sourceLocationArn = Lens.lens (\CanaryCodeOutput' {sourceLocationArn} -> sourceLocationArn) (\s@CanaryCodeOutput' {} a -> s {sourceLocationArn = a} :: CanaryCodeOutput)

-- | The entry point to use for the source code when running the canary.
canaryCodeOutput_handler :: Lens.Lens' CanaryCodeOutput (Prelude.Maybe Prelude.Text)
canaryCodeOutput_handler = Lens.lens (\CanaryCodeOutput' {handler} -> handler) (\s@CanaryCodeOutput' {} a -> s {handler = a} :: CanaryCodeOutput)

instance Data.FromJSON CanaryCodeOutput where
  parseJSON =
    Data.withObject
      "CanaryCodeOutput"
      ( \x ->
          CanaryCodeOutput'
            Prelude.<$> (x Data..:? "SourceLocationArn")
            Prelude.<*> (x Data..:? "Handler")
      )

instance Prelude.Hashable CanaryCodeOutput where
  hashWithSalt _salt CanaryCodeOutput' {..} =
    _salt `Prelude.hashWithSalt` sourceLocationArn
      `Prelude.hashWithSalt` handler

instance Prelude.NFData CanaryCodeOutput where
  rnf CanaryCodeOutput' {..} =
    Prelude.rnf sourceLocationArn
      `Prelude.seq` Prelude.rnf handler
