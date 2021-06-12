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
-- Module      : Network.AWS.Greengrass.Types.FunctionRunAsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionRunAsConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the user and group whose permissions are used when running the
-- Lambda function. You can specify one or both values to override the
-- default values. We recommend that you avoid running as root unless
-- absolutely necessary to minimize the risk of unintended changes or
-- malicious attacks. To run as root, you must set \'\'IsolationMode\'\' to
-- \'\'NoContainer\'\' and update config.json in
-- \'\'greengrass-root\/config\'\' to set \'\'allowFunctionsToRunAsRoot\'\'
-- to \'\'yes\'\'.
--
-- /See:/ 'newFunctionRunAsConfig' smart constructor.
data FunctionRunAsConfig = FunctionRunAsConfig'
  { -- | The group ID whose permissions are used to run a Lambda function.
    gid :: Core.Maybe Core.Int,
    -- | The user ID whose permissions are used to run a Lambda function.
    uid :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FunctionRunAsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gid', 'functionRunAsConfig_gid' - The group ID whose permissions are used to run a Lambda function.
--
-- 'uid', 'functionRunAsConfig_uid' - The user ID whose permissions are used to run a Lambda function.
newFunctionRunAsConfig ::
  FunctionRunAsConfig
newFunctionRunAsConfig =
  FunctionRunAsConfig'
    { gid = Core.Nothing,
      uid = Core.Nothing
    }

-- | The group ID whose permissions are used to run a Lambda function.
functionRunAsConfig_gid :: Lens.Lens' FunctionRunAsConfig (Core.Maybe Core.Int)
functionRunAsConfig_gid = Lens.lens (\FunctionRunAsConfig' {gid} -> gid) (\s@FunctionRunAsConfig' {} a -> s {gid = a} :: FunctionRunAsConfig)

-- | The user ID whose permissions are used to run a Lambda function.
functionRunAsConfig_uid :: Lens.Lens' FunctionRunAsConfig (Core.Maybe Core.Int)
functionRunAsConfig_uid = Lens.lens (\FunctionRunAsConfig' {uid} -> uid) (\s@FunctionRunAsConfig' {} a -> s {uid = a} :: FunctionRunAsConfig)

instance Core.FromJSON FunctionRunAsConfig where
  parseJSON =
    Core.withObject
      "FunctionRunAsConfig"
      ( \x ->
          FunctionRunAsConfig'
            Core.<$> (x Core..:? "Gid") Core.<*> (x Core..:? "Uid")
      )

instance Core.Hashable FunctionRunAsConfig

instance Core.NFData FunctionRunAsConfig

instance Core.ToJSON FunctionRunAsConfig where
  toJSON FunctionRunAsConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Gid" Core..=) Core.<$> gid,
            ("Uid" Core..=) Core.<$> uid
          ]
      )
