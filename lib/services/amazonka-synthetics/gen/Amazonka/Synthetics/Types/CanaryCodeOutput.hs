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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The entry point to use for the source code when running the canary.
    handler :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Lambda layer where Synthetics stores the canary script
    -- code.
    sourceLocationArn :: Prelude.Maybe Prelude.Text
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
-- 'handler', 'canaryCodeOutput_handler' - The entry point to use for the source code when running the canary.
--
-- 'sourceLocationArn', 'canaryCodeOutput_sourceLocationArn' - The ARN of the Lambda layer where Synthetics stores the canary script
-- code.
newCanaryCodeOutput ::
  CanaryCodeOutput
newCanaryCodeOutput =
  CanaryCodeOutput'
    { handler = Prelude.Nothing,
      sourceLocationArn = Prelude.Nothing
    }

-- | The entry point to use for the source code when running the canary.
canaryCodeOutput_handler :: Lens.Lens' CanaryCodeOutput (Prelude.Maybe Prelude.Text)
canaryCodeOutput_handler = Lens.lens (\CanaryCodeOutput' {handler} -> handler) (\s@CanaryCodeOutput' {} a -> s {handler = a} :: CanaryCodeOutput)

-- | The ARN of the Lambda layer where Synthetics stores the canary script
-- code.
canaryCodeOutput_sourceLocationArn :: Lens.Lens' CanaryCodeOutput (Prelude.Maybe Prelude.Text)
canaryCodeOutput_sourceLocationArn = Lens.lens (\CanaryCodeOutput' {sourceLocationArn} -> sourceLocationArn) (\s@CanaryCodeOutput' {} a -> s {sourceLocationArn = a} :: CanaryCodeOutput)

instance Data.FromJSON CanaryCodeOutput where
  parseJSON =
    Data.withObject
      "CanaryCodeOutput"
      ( \x ->
          CanaryCodeOutput'
            Prelude.<$> (x Data..:? "Handler")
            Prelude.<*> (x Data..:? "SourceLocationArn")
      )

instance Prelude.Hashable CanaryCodeOutput where
  hashWithSalt _salt CanaryCodeOutput' {..} =
    _salt
      `Prelude.hashWithSalt` handler
      `Prelude.hashWithSalt` sourceLocationArn

instance Prelude.NFData CanaryCodeOutput where
  rnf CanaryCodeOutput' {..} =
    Prelude.rnf handler `Prelude.seq`
      Prelude.rnf sourceLocationArn
