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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AnalyzerSummary where

import Amazonka.AccessAnalyzer.Types.AnalyzerStatus
import Amazonka.AccessAnalyzer.Types.StatusReason
import Amazonka.AccessAnalyzer.Types.Type
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the analyzer.
--
-- /See:/ 'newAnalyzerSummary' smart constructor.
data AnalyzerSummary = AnalyzerSummary'
  { -- | The tags added to the analyzer.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time at which the most recently analyzed resource was analyzed.
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
    -- | The ARN of the analyzer.
    arn :: Prelude.Text,
    -- | The name of the analyzer.
    name :: Prelude.Text,
    -- | The type of analyzer, which corresponds to the zone of trust chosen for
    -- the analyzer.
    type' :: Type,
    -- | A timestamp for the time at which the analyzer was created.
    createdAt :: Core.POSIX,
    -- | The status of the analyzer. An @Active@ analyzer successfully monitors
    -- supported resources and generates new findings. The analyzer is
    -- @Disabled@ when a user action, such as removing trusted access for
    -- Identity and Access Management Access Analyzer from Organizations,
    -- causes the analyzer to stop generating new findings. The status is
    -- @Creating@ when the analyzer creation is in progress and @Failed@ when
    -- the analyzer creation has failed.
    status :: AnalyzerStatus
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
-- 'tags', 'analyzerSummary_tags' - The tags added to the analyzer.
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
-- 'arn', 'analyzerSummary_arn' - The ARN of the analyzer.
--
-- 'name', 'analyzerSummary_name' - The name of the analyzer.
--
-- 'type'', 'analyzerSummary_type' - The type of analyzer, which corresponds to the zone of trust chosen for
-- the analyzer.
--
-- 'createdAt', 'analyzerSummary_createdAt' - A timestamp for the time at which the analyzer was created.
--
-- 'status', 'analyzerSummary_status' - The status of the analyzer. An @Active@ analyzer successfully monitors
-- supported resources and generates new findings. The analyzer is
-- @Disabled@ when a user action, such as removing trusted access for
-- Identity and Access Management Access Analyzer from Organizations,
-- causes the analyzer to stop generating new findings. The status is
-- @Creating@ when the analyzer creation is in progress and @Failed@ when
-- the analyzer creation has failed.
newAnalyzerSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Type ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'status'
  AnalyzerStatus ->
  AnalyzerSummary
newAnalyzerSummary
  pArn_
  pName_
  pType_
  pCreatedAt_
  pStatus_ =
    AnalyzerSummary'
      { tags = Prelude.Nothing,
        lastResourceAnalyzedAt = Prelude.Nothing,
        lastResourceAnalyzed = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        arn = pArn_,
        name = pName_,
        type' = pType_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        status = pStatus_
      }

-- | The tags added to the analyzer.
analyzerSummary_tags :: Lens.Lens' AnalyzerSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
analyzerSummary_tags = Lens.lens (\AnalyzerSummary' {tags} -> tags) (\s@AnalyzerSummary' {} a -> s {tags = a} :: AnalyzerSummary) Prelude.. Lens.mapping Lens.coerced

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

-- | The ARN of the analyzer.
analyzerSummary_arn :: Lens.Lens' AnalyzerSummary Prelude.Text
analyzerSummary_arn = Lens.lens (\AnalyzerSummary' {arn} -> arn) (\s@AnalyzerSummary' {} a -> s {arn = a} :: AnalyzerSummary)

-- | The name of the analyzer.
analyzerSummary_name :: Lens.Lens' AnalyzerSummary Prelude.Text
analyzerSummary_name = Lens.lens (\AnalyzerSummary' {name} -> name) (\s@AnalyzerSummary' {} a -> s {name = a} :: AnalyzerSummary)

-- | The type of analyzer, which corresponds to the zone of trust chosen for
-- the analyzer.
analyzerSummary_type :: Lens.Lens' AnalyzerSummary Type
analyzerSummary_type = Lens.lens (\AnalyzerSummary' {type'} -> type') (\s@AnalyzerSummary' {} a -> s {type' = a} :: AnalyzerSummary)

-- | A timestamp for the time at which the analyzer was created.
analyzerSummary_createdAt :: Lens.Lens' AnalyzerSummary Prelude.UTCTime
analyzerSummary_createdAt = Lens.lens (\AnalyzerSummary' {createdAt} -> createdAt) (\s@AnalyzerSummary' {} a -> s {createdAt = a} :: AnalyzerSummary) Prelude.. Core._Time

-- | The status of the analyzer. An @Active@ analyzer successfully monitors
-- supported resources and generates new findings. The analyzer is
-- @Disabled@ when a user action, such as removing trusted access for
-- Identity and Access Management Access Analyzer from Organizations,
-- causes the analyzer to stop generating new findings. The status is
-- @Creating@ when the analyzer creation is in progress and @Failed@ when
-- the analyzer creation has failed.
analyzerSummary_status :: Lens.Lens' AnalyzerSummary AnalyzerStatus
analyzerSummary_status = Lens.lens (\AnalyzerSummary' {status} -> status) (\s@AnalyzerSummary' {} a -> s {status = a} :: AnalyzerSummary)

instance Core.FromJSON AnalyzerSummary where
  parseJSON =
    Core.withObject
      "AnalyzerSummary"
      ( \x ->
          AnalyzerSummary'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "lastResourceAnalyzedAt")
            Prelude.<*> (x Core..:? "lastResourceAnalyzed")
            Prelude.<*> (x Core..:? "statusReason")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable AnalyzerSummary where
  hashWithSalt _salt AnalyzerSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastResourceAnalyzedAt
      `Prelude.hashWithSalt` lastResourceAnalyzed
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData AnalyzerSummary where
  rnf AnalyzerSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastResourceAnalyzedAt
      `Prelude.seq` Prelude.rnf lastResourceAnalyzed
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf status
