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
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig where

import Network.AWS.Greengrass.Types.FunctionIsolationMode
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'newFunctionDefaultExecutionConfig' smart constructor.
data FunctionDefaultExecutionConfig = FunctionDefaultExecutionConfig'
  { isolationMode :: Prelude.Maybe FunctionIsolationMode,
    runAs :: Prelude.Maybe FunctionRunAsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FunctionDefaultExecutionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isolationMode', 'functionDefaultExecutionConfig_isolationMode' - Undocumented member.
--
-- 'runAs', 'functionDefaultExecutionConfig_runAs' - Undocumented member.
newFunctionDefaultExecutionConfig ::
  FunctionDefaultExecutionConfig
newFunctionDefaultExecutionConfig =
  FunctionDefaultExecutionConfig'
    { isolationMode =
        Prelude.Nothing,
      runAs = Prelude.Nothing
    }

-- | Undocumented member.
functionDefaultExecutionConfig_isolationMode :: Lens.Lens' FunctionDefaultExecutionConfig (Prelude.Maybe FunctionIsolationMode)
functionDefaultExecutionConfig_isolationMode = Lens.lens (\FunctionDefaultExecutionConfig' {isolationMode} -> isolationMode) (\s@FunctionDefaultExecutionConfig' {} a -> s {isolationMode = a} :: FunctionDefaultExecutionConfig)

-- | Undocumented member.
functionDefaultExecutionConfig_runAs :: Lens.Lens' FunctionDefaultExecutionConfig (Prelude.Maybe FunctionRunAsConfig)
functionDefaultExecutionConfig_runAs = Lens.lens (\FunctionDefaultExecutionConfig' {runAs} -> runAs) (\s@FunctionDefaultExecutionConfig' {} a -> s {runAs = a} :: FunctionDefaultExecutionConfig)

instance
  Prelude.FromJSON
    FunctionDefaultExecutionConfig
  where
  parseJSON =
    Prelude.withObject
      "FunctionDefaultExecutionConfig"
      ( \x ->
          FunctionDefaultExecutionConfig'
            Prelude.<$> (x Prelude..:? "IsolationMode")
            Prelude.<*> (x Prelude..:? "RunAs")
      )

instance
  Prelude.Hashable
    FunctionDefaultExecutionConfig

instance
  Prelude.NFData
    FunctionDefaultExecutionConfig

instance
  Prelude.ToJSON
    FunctionDefaultExecutionConfig
  where
  toJSON FunctionDefaultExecutionConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IsolationMode" Prelude..=)
              Prelude.<$> isolationMode,
            ("RunAs" Prelude..=) Prelude.<$> runAs
          ]
      )
