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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.RecoveryGroupOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A Recovery Group generally containing multiple Cells
--
-- /See:/ 'newRecoveryGroupOutput' smart constructor.
data RecoveryGroupOutput = RecoveryGroupOutput'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The arn for the RecoveryGroup
    recoveryGroupArn :: Prelude.Text,
    -- | The name of the RecoveryGroup
    recoveryGroupName :: Prelude.Text,
    -- | A list of Cell arns
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
-- 'tags', 'recoveryGroupOutput_tags' - Undocumented member.
--
-- 'recoveryGroupArn', 'recoveryGroupOutput_recoveryGroupArn' - The arn for the RecoveryGroup
--
-- 'recoveryGroupName', 'recoveryGroupOutput_recoveryGroupName' - The name of the RecoveryGroup
--
-- 'cells', 'recoveryGroupOutput_cells' - A list of Cell arns
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

-- | Undocumented member.
recoveryGroupOutput_tags :: Lens.Lens' RecoveryGroupOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recoveryGroupOutput_tags = Lens.lens (\RecoveryGroupOutput' {tags} -> tags) (\s@RecoveryGroupOutput' {} a -> s {tags = a} :: RecoveryGroupOutput) Prelude.. Lens.mapping Lens.coerced

-- | The arn for the RecoveryGroup
recoveryGroupOutput_recoveryGroupArn :: Lens.Lens' RecoveryGroupOutput Prelude.Text
recoveryGroupOutput_recoveryGroupArn = Lens.lens (\RecoveryGroupOutput' {recoveryGroupArn} -> recoveryGroupArn) (\s@RecoveryGroupOutput' {} a -> s {recoveryGroupArn = a} :: RecoveryGroupOutput)

-- | The name of the RecoveryGroup
recoveryGroupOutput_recoveryGroupName :: Lens.Lens' RecoveryGroupOutput Prelude.Text
recoveryGroupOutput_recoveryGroupName = Lens.lens (\RecoveryGroupOutput' {recoveryGroupName} -> recoveryGroupName) (\s@RecoveryGroupOutput' {} a -> s {recoveryGroupName = a} :: RecoveryGroupOutput)

-- | A list of Cell arns
recoveryGroupOutput_cells :: Lens.Lens' RecoveryGroupOutput [Prelude.Text]
recoveryGroupOutput_cells = Lens.lens (\RecoveryGroupOutput' {cells} -> cells) (\s@RecoveryGroupOutput' {} a -> s {cells = a} :: RecoveryGroupOutput) Prelude.. Lens.coerced

instance Core.FromJSON RecoveryGroupOutput where
  parseJSON =
    Core.withObject
      "RecoveryGroupOutput"
      ( \x ->
          RecoveryGroupOutput'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "recoveryGroupArn")
            Prelude.<*> (x Core..: "recoveryGroupName")
            Prelude.<*> (x Core..:? "cells" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable RecoveryGroupOutput

instance Prelude.NFData RecoveryGroupOutput
