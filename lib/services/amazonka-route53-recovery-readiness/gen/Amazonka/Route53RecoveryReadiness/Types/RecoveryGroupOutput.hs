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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.RecoveryGroupOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.RecoveryGroupOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of the application, typically containing multiple
-- cells.
--
-- /See:/ 'newRecoveryGroupOutput' smart constructor.
data RecoveryGroupOutput = RecoveryGroupOutput'
  { -- | The tags associated with the recovery group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) for the recovery group.
    recoveryGroupArn :: Prelude.Text,
    -- | The name of the recovery group.
    recoveryGroupName :: Prelude.Text,
    -- | A list of a cell\'s Amazon Resource Names (ARNs).
    cells :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryGroupOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'recoveryGroupOutput_tags' - The tags associated with the recovery group.
--
-- 'recoveryGroupArn', 'recoveryGroupOutput_recoveryGroupArn' - The Amazon Resource Name (ARN) for the recovery group.
--
-- 'recoveryGroupName', 'recoveryGroupOutput_recoveryGroupName' - The name of the recovery group.
--
-- 'cells', 'recoveryGroupOutput_cells' - A list of a cell\'s Amazon Resource Names (ARNs).
newRecoveryGroupOutput ::
  -- | 'recoveryGroupArn'
  Prelude.Text ->
  -- | 'recoveryGroupName'
  Prelude.Text ->
  RecoveryGroupOutput
newRecoveryGroupOutput
  pRecoveryGroupArn_
  pRecoveryGroupName_ =
    RecoveryGroupOutput'
      { tags = Prelude.Nothing,
        recoveryGroupArn = pRecoveryGroupArn_,
        recoveryGroupName = pRecoveryGroupName_,
        cells = Prelude.mempty
      }

-- | The tags associated with the recovery group.
recoveryGroupOutput_tags :: Lens.Lens' RecoveryGroupOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recoveryGroupOutput_tags = Lens.lens (\RecoveryGroupOutput' {tags} -> tags) (\s@RecoveryGroupOutput' {} a -> s {tags = a} :: RecoveryGroupOutput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the recovery group.
recoveryGroupOutput_recoveryGroupArn :: Lens.Lens' RecoveryGroupOutput Prelude.Text
recoveryGroupOutput_recoveryGroupArn = Lens.lens (\RecoveryGroupOutput' {recoveryGroupArn} -> recoveryGroupArn) (\s@RecoveryGroupOutput' {} a -> s {recoveryGroupArn = a} :: RecoveryGroupOutput)

-- | The name of the recovery group.
recoveryGroupOutput_recoveryGroupName :: Lens.Lens' RecoveryGroupOutput Prelude.Text
recoveryGroupOutput_recoveryGroupName = Lens.lens (\RecoveryGroupOutput' {recoveryGroupName} -> recoveryGroupName) (\s@RecoveryGroupOutput' {} a -> s {recoveryGroupName = a} :: RecoveryGroupOutput)

-- | A list of a cell\'s Amazon Resource Names (ARNs).
recoveryGroupOutput_cells :: Lens.Lens' RecoveryGroupOutput [Prelude.Text]
recoveryGroupOutput_cells = Lens.lens (\RecoveryGroupOutput' {cells} -> cells) (\s@RecoveryGroupOutput' {} a -> s {cells = a} :: RecoveryGroupOutput) Prelude.. Lens.coerced

instance Data.FromJSON RecoveryGroupOutput where
  parseJSON =
    Data.withObject
      "RecoveryGroupOutput"
      ( \x ->
          RecoveryGroupOutput'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "recoveryGroupArn")
            Prelude.<*> (x Data..: "recoveryGroupName")
            Prelude.<*> (x Data..:? "cells" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RecoveryGroupOutput where
  hashWithSalt _salt RecoveryGroupOutput' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` recoveryGroupArn
      `Prelude.hashWithSalt` recoveryGroupName
      `Prelude.hashWithSalt` cells

instance Prelude.NFData RecoveryGroupOutput where
  rnf RecoveryGroupOutput' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf recoveryGroupArn
      `Prelude.seq` Prelude.rnf recoveryGroupName
      `Prelude.seq` Prelude.rnf cells
