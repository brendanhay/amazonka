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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbParameterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a parameter group for a DB instance.
--
-- /See:/ 'newAwsRdsDbParameterGroup' smart constructor.
data AwsRdsDbParameterGroup = AwsRdsDbParameterGroup'
  { -- | The name of the parameter group.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of parameter updates.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroupName', 'awsRdsDbParameterGroup_dbParameterGroupName' - The name of the parameter group.
--
-- 'parameterApplyStatus', 'awsRdsDbParameterGroup_parameterApplyStatus' - The status of parameter updates.
newAwsRdsDbParameterGroup ::
  AwsRdsDbParameterGroup
newAwsRdsDbParameterGroup =
  AwsRdsDbParameterGroup'
    { dbParameterGroupName =
        Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing
    }

-- | The name of the parameter group.
awsRdsDbParameterGroup_dbParameterGroupName :: Lens.Lens' AwsRdsDbParameterGroup (Prelude.Maybe Prelude.Text)
awsRdsDbParameterGroup_dbParameterGroupName = Lens.lens (\AwsRdsDbParameterGroup' {dbParameterGroupName} -> dbParameterGroupName) (\s@AwsRdsDbParameterGroup' {} a -> s {dbParameterGroupName = a} :: AwsRdsDbParameterGroup)

-- | The status of parameter updates.
awsRdsDbParameterGroup_parameterApplyStatus :: Lens.Lens' AwsRdsDbParameterGroup (Prelude.Maybe Prelude.Text)
awsRdsDbParameterGroup_parameterApplyStatus = Lens.lens (\AwsRdsDbParameterGroup' {parameterApplyStatus} -> parameterApplyStatus) (\s@AwsRdsDbParameterGroup' {} a -> s {parameterApplyStatus = a} :: AwsRdsDbParameterGroup)

instance Data.FromJSON AwsRdsDbParameterGroup where
  parseJSON =
    Data.withObject
      "AwsRdsDbParameterGroup"
      ( \x ->
          AwsRdsDbParameterGroup'
            Prelude.<$> (x Data..:? "DbParameterGroupName")
            Prelude.<*> (x Data..:? "ParameterApplyStatus")
      )

instance Prelude.Hashable AwsRdsDbParameterGroup where
  hashWithSalt _salt AwsRdsDbParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` parameterApplyStatus

instance Prelude.NFData AwsRdsDbParameterGroup where
  rnf AwsRdsDbParameterGroup' {..} =
    Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf parameterApplyStatus

instance Data.ToJSON AwsRdsDbParameterGroup where
  toJSON AwsRdsDbParameterGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DbParameterGroupName" Data..=)
              Prelude.<$> dbParameterGroupName,
            ("ParameterApplyStatus" Data..=)
              Prelude.<$> parameterApplyStatus
          ]
      )
