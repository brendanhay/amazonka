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
-- Module      : Network.AWS.Greengrass.Types.FunctionExecutionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionExecutionConfig where

import Network.AWS.Greengrass.Types.FunctionIsolationMode
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'newFunctionExecutionConfig' smart constructor.
data FunctionExecutionConfig = FunctionExecutionConfig'
  { isolationMode :: Prelude.Maybe FunctionIsolationMode,
    runAs :: Prelude.Maybe FunctionRunAsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FunctionExecutionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isolationMode', 'functionExecutionConfig_isolationMode' - Undocumented member.
--
-- 'runAs', 'functionExecutionConfig_runAs' - Undocumented member.
newFunctionExecutionConfig ::
  FunctionExecutionConfig
newFunctionExecutionConfig =
  FunctionExecutionConfig'
    { isolationMode =
        Prelude.Nothing,
      runAs = Prelude.Nothing
    }

-- | Undocumented member.
functionExecutionConfig_isolationMode :: Lens.Lens' FunctionExecutionConfig (Prelude.Maybe FunctionIsolationMode)
functionExecutionConfig_isolationMode = Lens.lens (\FunctionExecutionConfig' {isolationMode} -> isolationMode) (\s@FunctionExecutionConfig' {} a -> s {isolationMode = a} :: FunctionExecutionConfig)

-- | Undocumented member.
functionExecutionConfig_runAs :: Lens.Lens' FunctionExecutionConfig (Prelude.Maybe FunctionRunAsConfig)
functionExecutionConfig_runAs = Lens.lens (\FunctionExecutionConfig' {runAs} -> runAs) (\s@FunctionExecutionConfig' {} a -> s {runAs = a} :: FunctionExecutionConfig)

instance Prelude.FromJSON FunctionExecutionConfig where
  parseJSON =
    Prelude.withObject
      "FunctionExecutionConfig"
      ( \x ->
          FunctionExecutionConfig'
            Prelude.<$> (x Prelude..:? "IsolationMode")
            Prelude.<*> (x Prelude..:? "RunAs")
      )

instance Prelude.Hashable FunctionExecutionConfig

instance Prelude.NFData FunctionExecutionConfig

instance Prelude.ToJSON FunctionExecutionConfig where
  toJSON FunctionExecutionConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IsolationMode" Prelude..=)
              Prelude.<$> isolationMode,
            ("RunAs" Prelude..=) Prelude.<$> runAs
          ]
      )
