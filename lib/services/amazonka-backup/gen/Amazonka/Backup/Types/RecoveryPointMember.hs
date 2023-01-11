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
-- Module      : Amazonka.Backup.Types.RecoveryPointMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.RecoveryPointMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This is a recovery point which is a child (nested) recovery point of a
-- parent (composite) recovery point. These recovery points can be
-- disassociated from their parent (composite) recovery point, in which
-- case they will no longer be a member.
--
-- /See:/ 'newRecoveryPointMember' smart constructor.
data RecoveryPointMember = RecoveryPointMember'
  { -- | This is the Amazon Resource Name (ARN) of the parent (composite)
    -- recovery point.
    recoveryPointArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryPointMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryPointArn', 'recoveryPointMember_recoveryPointArn' - This is the Amazon Resource Name (ARN) of the parent (composite)
-- recovery point.
newRecoveryPointMember ::
  RecoveryPointMember
newRecoveryPointMember =
  RecoveryPointMember'
    { recoveryPointArn =
        Prelude.Nothing
    }

-- | This is the Amazon Resource Name (ARN) of the parent (composite)
-- recovery point.
recoveryPointMember_recoveryPointArn :: Lens.Lens' RecoveryPointMember (Prelude.Maybe Prelude.Text)
recoveryPointMember_recoveryPointArn = Lens.lens (\RecoveryPointMember' {recoveryPointArn} -> recoveryPointArn) (\s@RecoveryPointMember' {} a -> s {recoveryPointArn = a} :: RecoveryPointMember)

instance Data.FromJSON RecoveryPointMember where
  parseJSON =
    Data.withObject
      "RecoveryPointMember"
      ( \x ->
          RecoveryPointMember'
            Prelude.<$> (x Data..:? "RecoveryPointArn")
      )

instance Prelude.Hashable RecoveryPointMember where
  hashWithSalt _salt RecoveryPointMember' {..} =
    _salt `Prelude.hashWithSalt` recoveryPointArn

instance Prelude.NFData RecoveryPointMember where
  rnf RecoveryPointMember' {..} =
    Prelude.rnf recoveryPointArn
