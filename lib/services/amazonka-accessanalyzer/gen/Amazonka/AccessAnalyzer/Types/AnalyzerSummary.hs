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
-- Module      : Amazonka.AccessAnalyzer.Types.AnalyzerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AnalyzerSummary where

import Amazonka.AccessAnalyzer.Types.AnalyzerStatus
import Amazonka.AccessAnalyzer.Types.StatusReason
import Amazonka.AccessAnalyzer.Types.Type
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the analyzer.
--
-- /See:/ 'newAnalyzerSummary' smart constructor.
data AnalyzerSummary = AnalyzerSummary'
  { -- | The time at which the most recently analyzed resource was analyzed.
    lastResourceAnalyzedAt :: Prelude.Maybe Core.POSIX,
    -- | The resource that was most recently analyzed by the analyzer.
    lastResourceAnalyzed :: Prelude.Maybe Prelude.Text,
    -- | The @statusReason@ provides more details about the current status of the
    -- analyzer. For example, if the creation for the analyzer fails, a
    -- @Failed@ status is returned. For an analyzer with organization as the
    -- type, this failure can be due to an issue with creating the
    -- service-linked roles required in the member accounts of the Amazon Web
    -- Services organization.
    statusReason :: Prelude.Maybe StatusReason,
    -- | The tags added to the analyzer.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the analyzer.
    arn :: Prelude.Text,
    -- | A timestamp for the time at which the analyzer was created.
    createdAt :: Core.POSIX,
    -- | The name of the analyzer.
    name :: Prelude.Text,
    -- | The status of the analyzer. An @Active@ analyzer successfully monitors
    -- supported resources and generates new findings. The analyzer is
    -- @Disabled@ when a user action, such as removing trusted access for
    -- Identity and Access Management Access Analyzer from Organizations,
    -- causes the analyzer to stop generating new findings. The status is
    -- @Creating@ when the analyzer creation is in progress and @Failed@ when
    -- the analyzer creation has failed.
    status :: AnalyzerStatus,
    -- | The type of analyzer, which corresponds to the zone of trust chosen for
    -- the analyzer.
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastResourceAnalyzedAt', 'analyzerSummary_lastResourceAnalyzedAt' - The time at which the most recently analyzed resource was analyzed.
--
-- 'lastResourceAnalyzed', 'analyzerSummary_lastResourceAnalyzed' - The resource that was most recently analyzed by the analyzer.
--
-- 'statusReason', 'analyzerSummary_statusReason' - The @statusReason@ provides more details about the current status of the
-- analyzer. For example, if the creation for the analyzer fails, a
-- @Failed@ status is returned. For an analyzer with organization as the
-- type, this failure can be due to an issue with creating the
-- service-linked roles required in the member accounts of the Amazon Web
-- Services organization.
--
-- 'tags', 'analyzerSummary_tags' - The tags added to the analyzer.
--
-- 'arn', 'analyzerSummary_arn' - The ARN of the analyzer.
--
-- 'createdAt', 'analyzerSummary_createdAt' - A timestamp for the time at which the analyzer was created.
--
-- 'name', 'analyzerSummary_name' - The name of the analyzer.
--
-- 'status', 'analyzerSummary_status' - The status of the analyzer. An @Active@ analyzer successfully monitors
-- supported resources and generates new findings. The analyzer is
-- @Disabled@ when a user action, such as removing trusted access for
-- Identity and Access Management Access Analyzer from Organizations,
-- causes the analyzer to stop generating new findings. The status is
-- @Creating@ when the analyzer creation is in progress and @Failed@ when
-- the analyzer creation has failed.
--
-- 'type'', 'analyzerSummary_type' - The type of analyzer, which corresponds to the zone of trust chosen for
-- the analyzer.
newAnalyzerSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  AnalyzerStatus ->
  -- | 'type''
  Type ->
  AnalyzerSummary
newAnalyzerSummary
  pArn_
  pCreatedAt_
  pName_
  pStatus_
  pType_ =
    AnalyzerSummary'
      { lastResourceAnalyzedAt =
          Prelude.Nothing,
        lastResourceAnalyzed = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        name = pName_,
        status = pStatus_,
        type' = pType_
      }

-- | The time at which the most recently analyzed resource was analyzed.
analyzerSummary_lastResourceAnalyzedAt :: Lens.Lens' AnalyzerSummary (Prelude.Maybe Prelude.UTCTime)
analyzerSummary_lastResourceAnalyzedAt = Lens.lens (\AnalyzerSummary' {lastResourceAnalyzedAt} -> lastResourceAnalyzedAt) (\s@AnalyzerSummary' {} a -> s {lastResourceAnalyzedAt = a} :: AnalyzerSummary) Prelude.. Lens.mapping Core._Time

-- | The resource that was most recently analyzed by the analyzer.
analyzerSummary_lastResourceAnalyzed :: Lens.Lens' AnalyzerSummary (Prelude.Maybe Prelude.Text)
analyzerSummary_lastResourceAnalyzed = Lens.lens (\AnalyzerSummary' {lastResourceAnalyzed} -> lastResourceAnalyzed) (\s@AnalyzerSummary' {} a -> s {lastResourceAnalyzed = a} :: AnalyzerSummary)

-- | The @statusReason@ provides more details about the current status of the
-- analyzer. For example, if the creation for the analyzer fails, a
-- @Failed@ status is returned. For an analyzer with organization as the
-- type, this failure can be due to an issue with creating the
-- service-linked roles required in the member accounts of the Amazon Web
-- Services organization.
analyzerSummary_statusReason :: Lens.Lens' AnalyzerSummary (Prelude.Maybe StatusReason)
analyzerSummary_statusReason = Lens.lens (\AnalyzerSummary' {statusReason} -> statusReason) (\s@AnalyzerSummary' {} a -> s {statusReason = a} :: AnalyzerSummary)

-- | The tags added to the analyzer.
analyzerSummary_tags :: Lens.Lens' AnalyzerSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
analyzerSummary_tags = Lens.lens (\AnalyzerSummary' {tags} -> tags) (\s@AnalyzerSummary' {} a -> s {tags = a} :: AnalyzerSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the analyzer.
analyzerSummary_arn :: Lens.Lens' AnalyzerSummary Prelude.Text
analyzerSummary_arn = Lens.lens (\AnalyzerSummary' {arn} -> arn) (\s@AnalyzerSummary' {} a -> s {arn = a} :: AnalyzerSummary)

-- | A timestamp for the time at which the analyzer was created.
analyzerSummary_createdAt :: Lens.Lens' AnalyzerSummary Prelude.UTCTime
analyzerSummary_createdAt = Lens.lens (\AnalyzerSummary' {createdAt} -> createdAt) (\s@AnalyzerSummary' {} a -> s {createdAt = a} :: AnalyzerSummary) Prelude.. Core._Time

-- | The name of the analyzer.
analyzerSummary_name :: Lens.Lens' AnalyzerSummary Prelude.Text
analyzerSummary_name = Lens.lens (\AnalyzerSummary' {name} -> name) (\s@AnalyzerSummary' {} a -> s {name = a} :: AnalyzerSummary)

-- | The status of the analyzer. An @Active@ analyzer successfully monitors
-- supported resources and generates new findings. The analyzer is
-- @Disabled@ when a user action, such as removing trusted access for
-- Identity and Access Management Access Analyzer from Organizations,
-- causes the analyzer to stop generating new findings. The status is
-- @Creating@ when the analyzer creation is in progress and @Failed@ when
-- the analyzer creation has failed.
analyzerSummary_status :: Lens.Lens' AnalyzerSummary AnalyzerStatus
analyzerSummary_status = Lens.lens (\AnalyzerSummary' {status} -> status) (\s@AnalyzerSummary' {} a -> s {status = a} :: AnalyzerSummary)

-- | The type of analyzer, which corresponds to the zone of trust chosen for
-- the analyzer.
analyzerSummary_type :: Lens.Lens' AnalyzerSummary Type
analyzerSummary_type = Lens.lens (\AnalyzerSummary' {type'} -> type') (\s@AnalyzerSummary' {} a -> s {type' = a} :: AnalyzerSummary)

instance Core.FromJSON AnalyzerSummary where
  parseJSON =
    Core.withObject
      "AnalyzerSummary"
      ( \x ->
          AnalyzerSummary'
            Prelude.<$> (x Core..:? "lastResourceAnalyzedAt")
            Prelude.<*> (x Core..:? "lastResourceAnalyzed")
            Prelude.<*> (x Core..:? "statusReason")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable AnalyzerSummary where
  hashWithSalt _salt AnalyzerSummary' {..} =
    _salt `Prelude.hashWithSalt` lastResourceAnalyzedAt
      `Prelude.hashWithSalt` lastResourceAnalyzed
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AnalyzerSummary where
  rnf AnalyzerSummary' {..} =
    Prelude.rnf lastResourceAnalyzedAt
      `Prelude.seq` Prelude.rnf lastResourceAnalyzed
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
