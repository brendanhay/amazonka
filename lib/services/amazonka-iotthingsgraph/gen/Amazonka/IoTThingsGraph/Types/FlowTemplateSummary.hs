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
-- Module      : Amazonka.IoTThingsGraph.Types.FlowTemplateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.FlowTemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains summary information about a workflow.
--
-- /See:/ 'newFlowTemplateSummary' smart constructor.
data FlowTemplateSummary = FlowTemplateSummary'
  { -- | The ARN of the workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the workflow was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The revision number of the workflow.
    revisionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the workflow.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'flowTemplateSummary_arn' - The ARN of the workflow.
--
-- 'createdAt', 'flowTemplateSummary_createdAt' - The date when the workflow was created.
--
-- 'revisionNumber', 'flowTemplateSummary_revisionNumber' - The revision number of the workflow.
--
-- 'id', 'flowTemplateSummary_id' - The ID of the workflow.
newFlowTemplateSummary ::
  FlowTemplateSummary
newFlowTemplateSummary =
  FlowTemplateSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      revisionNumber = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The ARN of the workflow.
flowTemplateSummary_arn :: Lens.Lens' FlowTemplateSummary (Prelude.Maybe Prelude.Text)
flowTemplateSummary_arn = Lens.lens (\FlowTemplateSummary' {arn} -> arn) (\s@FlowTemplateSummary' {} a -> s {arn = a} :: FlowTemplateSummary)

-- | The date when the workflow was created.
flowTemplateSummary_createdAt :: Lens.Lens' FlowTemplateSummary (Prelude.Maybe Prelude.UTCTime)
flowTemplateSummary_createdAt = Lens.lens (\FlowTemplateSummary' {createdAt} -> createdAt) (\s@FlowTemplateSummary' {} a -> s {createdAt = a} :: FlowTemplateSummary) Prelude.. Lens.mapping Core._Time

-- | The revision number of the workflow.
flowTemplateSummary_revisionNumber :: Lens.Lens' FlowTemplateSummary (Prelude.Maybe Prelude.Integer)
flowTemplateSummary_revisionNumber = Lens.lens (\FlowTemplateSummary' {revisionNumber} -> revisionNumber) (\s@FlowTemplateSummary' {} a -> s {revisionNumber = a} :: FlowTemplateSummary)

-- | The ID of the workflow.
flowTemplateSummary_id :: Lens.Lens' FlowTemplateSummary (Prelude.Maybe Prelude.Text)
flowTemplateSummary_id = Lens.lens (\FlowTemplateSummary' {id} -> id) (\s@FlowTemplateSummary' {} a -> s {id = a} :: FlowTemplateSummary)

instance Core.FromJSON FlowTemplateSummary where
  parseJSON =
    Core.withObject
      "FlowTemplateSummary"
      ( \x ->
          FlowTemplateSummary'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "revisionNumber")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable FlowTemplateSummary where
  hashWithSalt _salt FlowTemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` revisionNumber
      `Prelude.hashWithSalt` id

instance Prelude.NFData FlowTemplateSummary where
  rnf FlowTemplateSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf revisionNumber
      `Prelude.seq` Prelude.rnf id
