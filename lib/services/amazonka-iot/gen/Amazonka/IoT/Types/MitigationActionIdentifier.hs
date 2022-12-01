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
-- Module      : Amazonka.IoT.Types.MitigationActionIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MitigationActionIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information that identifies a mitigation action. This information is
-- returned by ListMitigationActions.
--
-- /See:/ 'newMitigationActionIdentifier' smart constructor.
data MitigationActionIdentifier = MitigationActionIdentifier'
  { -- | The friendly name of the mitigation action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The date when this mitigation action was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The IAM role ARN used to apply this mitigation action.
    actionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MitigationActionIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'mitigationActionIdentifier_actionName' - The friendly name of the mitigation action.
--
-- 'creationDate', 'mitigationActionIdentifier_creationDate' - The date when this mitigation action was created.
--
-- 'actionArn', 'mitigationActionIdentifier_actionArn' - The IAM role ARN used to apply this mitigation action.
newMitigationActionIdentifier ::
  MitigationActionIdentifier
newMitigationActionIdentifier =
  MitigationActionIdentifier'
    { actionName =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      actionArn = Prelude.Nothing
    }

-- | The friendly name of the mitigation action.
mitigationActionIdentifier_actionName :: Lens.Lens' MitigationActionIdentifier (Prelude.Maybe Prelude.Text)
mitigationActionIdentifier_actionName = Lens.lens (\MitigationActionIdentifier' {actionName} -> actionName) (\s@MitigationActionIdentifier' {} a -> s {actionName = a} :: MitigationActionIdentifier)

-- | The date when this mitigation action was created.
mitigationActionIdentifier_creationDate :: Lens.Lens' MitigationActionIdentifier (Prelude.Maybe Prelude.UTCTime)
mitigationActionIdentifier_creationDate = Lens.lens (\MitigationActionIdentifier' {creationDate} -> creationDate) (\s@MitigationActionIdentifier' {} a -> s {creationDate = a} :: MitigationActionIdentifier) Prelude.. Lens.mapping Core._Time

-- | The IAM role ARN used to apply this mitigation action.
mitigationActionIdentifier_actionArn :: Lens.Lens' MitigationActionIdentifier (Prelude.Maybe Prelude.Text)
mitigationActionIdentifier_actionArn = Lens.lens (\MitigationActionIdentifier' {actionArn} -> actionArn) (\s@MitigationActionIdentifier' {} a -> s {actionArn = a} :: MitigationActionIdentifier)

instance Core.FromJSON MitigationActionIdentifier where
  parseJSON =
    Core.withObject
      "MitigationActionIdentifier"
      ( \x ->
          MitigationActionIdentifier'
            Prelude.<$> (x Core..:? "actionName")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "actionArn")
      )

instance Prelude.Hashable MitigationActionIdentifier where
  hashWithSalt _salt MitigationActionIdentifier' {..} =
    _salt `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` actionArn

instance Prelude.NFData MitigationActionIdentifier where
  rnf MitigationActionIdentifier' {..} =
    Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf actionArn
