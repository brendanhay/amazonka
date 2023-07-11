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
-- Module      : Amazonka.Greengrass.Types.FunctionRunAsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.FunctionRunAsConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    gid :: Prelude.Maybe Prelude.Int,
    -- | The user ID whose permissions are used to run a Lambda function.
    uid :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { gid = Prelude.Nothing,
      uid = Prelude.Nothing
    }

-- | The group ID whose permissions are used to run a Lambda function.
functionRunAsConfig_gid :: Lens.Lens' FunctionRunAsConfig (Prelude.Maybe Prelude.Int)
functionRunAsConfig_gid = Lens.lens (\FunctionRunAsConfig' {gid} -> gid) (\s@FunctionRunAsConfig' {} a -> s {gid = a} :: FunctionRunAsConfig)

-- | The user ID whose permissions are used to run a Lambda function.
functionRunAsConfig_uid :: Lens.Lens' FunctionRunAsConfig (Prelude.Maybe Prelude.Int)
functionRunAsConfig_uid = Lens.lens (\FunctionRunAsConfig' {uid} -> uid) (\s@FunctionRunAsConfig' {} a -> s {uid = a} :: FunctionRunAsConfig)

instance Data.FromJSON FunctionRunAsConfig where
  parseJSON =
    Data.withObject
      "FunctionRunAsConfig"
      ( \x ->
          FunctionRunAsConfig'
            Prelude.<$> (x Data..:? "Gid")
            Prelude.<*> (x Data..:? "Uid")
      )

instance Prelude.Hashable FunctionRunAsConfig where
  hashWithSalt _salt FunctionRunAsConfig' {..} =
    _salt
      `Prelude.hashWithSalt` gid
      `Prelude.hashWithSalt` uid

instance Prelude.NFData FunctionRunAsConfig where
  rnf FunctionRunAsConfig' {..} =
    Prelude.rnf gid `Prelude.seq` Prelude.rnf uid

instance Data.ToJSON FunctionRunAsConfig where
  toJSON FunctionRunAsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Gid" Data..=) Prelude.<$> gid,
            ("Uid" Data..=) Prelude.<$> uid
          ]
      )
