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
-- Module      : Amazonka.GreengrassV2.Types.LambdaLinuxProcessParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.LambdaLinuxProcessParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.LambdaContainerParams
import Amazonka.GreengrassV2.Types.LambdaIsolationMode
import qualified Amazonka.Prelude as Prelude

-- | Contains parameters for a Linux process that contains an Lambda
-- function.
--
-- /See:/ 'newLambdaLinuxProcessParams' smart constructor.
data LambdaLinuxProcessParams = LambdaLinuxProcessParams'
  { -- | The parameters for the container in which the Lambda function runs.
    containerParams :: Prelude.Maybe LambdaContainerParams,
    -- | The isolation mode for the process that contains the Lambda function.
    -- The process can run in an isolated runtime environment inside the IoT
    -- Greengrass container, or as a regular process outside any container.
    --
    -- Default: @GreengrassContainer@
    isolationMode :: Prelude.Maybe LambdaIsolationMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaLinuxProcessParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerParams', 'lambdaLinuxProcessParams_containerParams' - The parameters for the container in which the Lambda function runs.
--
-- 'isolationMode', 'lambdaLinuxProcessParams_isolationMode' - The isolation mode for the process that contains the Lambda function.
-- The process can run in an isolated runtime environment inside the IoT
-- Greengrass container, or as a regular process outside any container.
--
-- Default: @GreengrassContainer@
newLambdaLinuxProcessParams ::
  LambdaLinuxProcessParams
newLambdaLinuxProcessParams =
  LambdaLinuxProcessParams'
    { containerParams =
        Prelude.Nothing,
      isolationMode = Prelude.Nothing
    }

-- | The parameters for the container in which the Lambda function runs.
lambdaLinuxProcessParams_containerParams :: Lens.Lens' LambdaLinuxProcessParams (Prelude.Maybe LambdaContainerParams)
lambdaLinuxProcessParams_containerParams = Lens.lens (\LambdaLinuxProcessParams' {containerParams} -> containerParams) (\s@LambdaLinuxProcessParams' {} a -> s {containerParams = a} :: LambdaLinuxProcessParams)

-- | The isolation mode for the process that contains the Lambda function.
-- The process can run in an isolated runtime environment inside the IoT
-- Greengrass container, or as a regular process outside any container.
--
-- Default: @GreengrassContainer@
lambdaLinuxProcessParams_isolationMode :: Lens.Lens' LambdaLinuxProcessParams (Prelude.Maybe LambdaIsolationMode)
lambdaLinuxProcessParams_isolationMode = Lens.lens (\LambdaLinuxProcessParams' {isolationMode} -> isolationMode) (\s@LambdaLinuxProcessParams' {} a -> s {isolationMode = a} :: LambdaLinuxProcessParams)

instance Prelude.Hashable LambdaLinuxProcessParams where
  hashWithSalt _salt LambdaLinuxProcessParams' {..} =
    _salt
      `Prelude.hashWithSalt` containerParams
      `Prelude.hashWithSalt` isolationMode

instance Prelude.NFData LambdaLinuxProcessParams where
  rnf LambdaLinuxProcessParams' {..} =
    Prelude.rnf containerParams
      `Prelude.seq` Prelude.rnf isolationMode

instance Data.ToJSON LambdaLinuxProcessParams where
  toJSON LambdaLinuxProcessParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerParams" Data..=)
              Prelude.<$> containerParams,
            ("isolationMode" Data..=) Prelude.<$> isolationMode
          ]
      )
