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
-- Module      : Amazonka.NetworkManager.Types.ProposedSegmentChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ProposedSegmentChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a proposed segment change. In some cases, the segment change
-- must first be evaluated and accepted.
--
-- /See:/ 'newProposedSegmentChange' smart constructor.
data ProposedSegmentChange = ProposedSegmentChange'
  { -- | The rule number in the policy document that applies to this change.
    attachmentPolicyRuleNumber :: Prelude.Maybe Prelude.Int,
    -- | The name of the segment to change.
    segmentName :: Prelude.Maybe Prelude.Text,
    -- | The list of key-value tags that changed for the segment.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProposedSegmentChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentPolicyRuleNumber', 'proposedSegmentChange_attachmentPolicyRuleNumber' - The rule number in the policy document that applies to this change.
--
-- 'segmentName', 'proposedSegmentChange_segmentName' - The name of the segment to change.
--
-- 'tags', 'proposedSegmentChange_tags' - The list of key-value tags that changed for the segment.
newProposedSegmentChange ::
  ProposedSegmentChange
newProposedSegmentChange =
  ProposedSegmentChange'
    { attachmentPolicyRuleNumber =
        Prelude.Nothing,
      segmentName = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The rule number in the policy document that applies to this change.
proposedSegmentChange_attachmentPolicyRuleNumber :: Lens.Lens' ProposedSegmentChange (Prelude.Maybe Prelude.Int)
proposedSegmentChange_attachmentPolicyRuleNumber = Lens.lens (\ProposedSegmentChange' {attachmentPolicyRuleNumber} -> attachmentPolicyRuleNumber) (\s@ProposedSegmentChange' {} a -> s {attachmentPolicyRuleNumber = a} :: ProposedSegmentChange)

-- | The name of the segment to change.
proposedSegmentChange_segmentName :: Lens.Lens' ProposedSegmentChange (Prelude.Maybe Prelude.Text)
proposedSegmentChange_segmentName = Lens.lens (\ProposedSegmentChange' {segmentName} -> segmentName) (\s@ProposedSegmentChange' {} a -> s {segmentName = a} :: ProposedSegmentChange)

-- | The list of key-value tags that changed for the segment.
proposedSegmentChange_tags :: Lens.Lens' ProposedSegmentChange (Prelude.Maybe [Tag])
proposedSegmentChange_tags = Lens.lens (\ProposedSegmentChange' {tags} -> tags) (\s@ProposedSegmentChange' {} a -> s {tags = a} :: ProposedSegmentChange) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ProposedSegmentChange where
  parseJSON =
    Data.withObject
      "ProposedSegmentChange"
      ( \x ->
          ProposedSegmentChange'
            Prelude.<$> (x Data..:? "AttachmentPolicyRuleNumber")
            Prelude.<*> (x Data..:? "SegmentName")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ProposedSegmentChange where
  hashWithSalt _salt ProposedSegmentChange' {..} =
    _salt
      `Prelude.hashWithSalt` attachmentPolicyRuleNumber
      `Prelude.hashWithSalt` segmentName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ProposedSegmentChange where
  rnf ProposedSegmentChange' {..} =
    Prelude.rnf attachmentPolicyRuleNumber
      `Prelude.seq` Prelude.rnf segmentName
      `Prelude.seq` Prelude.rnf tags
