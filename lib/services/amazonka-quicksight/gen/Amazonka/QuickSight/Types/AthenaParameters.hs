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
-- Module      : Amazonka.QuickSight.Types.AthenaParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AthenaParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters for Amazon Athena.
--
-- /See:/ 'newAthenaParameters' smart constructor.
data AthenaParameters = AthenaParameters'
  { -- | Use the @RoleArn@ structure to override an account-wide role for a
    -- specific Athena data source. For example, say an account administrator
    -- has turned off all Athena access with an account-wide role. The
    -- administrator can then use @RoleArn@ to bypass the account-wide role and
    -- allow Athena access for the single Athena data source that is specified
    -- in the structure, even if the account-wide role forbidding Athena access
    -- is still active.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The workgroup that Amazon Athena uses.
    workGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AthenaParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'athenaParameters_roleArn' - Use the @RoleArn@ structure to override an account-wide role for a
-- specific Athena data source. For example, say an account administrator
-- has turned off all Athena access with an account-wide role. The
-- administrator can then use @RoleArn@ to bypass the account-wide role and
-- allow Athena access for the single Athena data source that is specified
-- in the structure, even if the account-wide role forbidding Athena access
-- is still active.
--
-- 'workGroup', 'athenaParameters_workGroup' - The workgroup that Amazon Athena uses.
newAthenaParameters ::
  AthenaParameters
newAthenaParameters =
  AthenaParameters'
    { roleArn = Prelude.Nothing,
      workGroup = Prelude.Nothing
    }

-- | Use the @RoleArn@ structure to override an account-wide role for a
-- specific Athena data source. For example, say an account administrator
-- has turned off all Athena access with an account-wide role. The
-- administrator can then use @RoleArn@ to bypass the account-wide role and
-- allow Athena access for the single Athena data source that is specified
-- in the structure, even if the account-wide role forbidding Athena access
-- is still active.
athenaParameters_roleArn :: Lens.Lens' AthenaParameters (Prelude.Maybe Prelude.Text)
athenaParameters_roleArn = Lens.lens (\AthenaParameters' {roleArn} -> roleArn) (\s@AthenaParameters' {} a -> s {roleArn = a} :: AthenaParameters)

-- | The workgroup that Amazon Athena uses.
athenaParameters_workGroup :: Lens.Lens' AthenaParameters (Prelude.Maybe Prelude.Text)
athenaParameters_workGroup = Lens.lens (\AthenaParameters' {workGroup} -> workGroup) (\s@AthenaParameters' {} a -> s {workGroup = a} :: AthenaParameters)

instance Data.FromJSON AthenaParameters where
  parseJSON =
    Data.withObject
      "AthenaParameters"
      ( \x ->
          AthenaParameters'
            Prelude.<$> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "WorkGroup")
      )

instance Prelude.Hashable AthenaParameters where
  hashWithSalt _salt AthenaParameters' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData AthenaParameters where
  rnf AthenaParameters' {..} =
    Prelude.rnf roleArn `Prelude.seq`
      Prelude.rnf workGroup

instance Data.ToJSON AthenaParameters where
  toJSON AthenaParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("WorkGroup" Data..=) Prelude.<$> workGroup
          ]
      )
