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
-- Module      : Network.AWS.FMS.Types.PartialMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PartialMatch where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The reference rule that partially matches the @ViolationTarget@ rule and
-- violation reason.
--
-- /See:/ 'newPartialMatch' smart constructor.
data PartialMatch = PartialMatch'
  { -- | The violation reason.
    targetViolationReasons :: Core.Maybe [Core.Text],
    -- | The reference rule from the master security group of the AWS Firewall
    -- Manager policy.
    reference :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PartialMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetViolationReasons', 'partialMatch_targetViolationReasons' - The violation reason.
--
-- 'reference', 'partialMatch_reference' - The reference rule from the master security group of the AWS Firewall
-- Manager policy.
newPartialMatch ::
  PartialMatch
newPartialMatch =
  PartialMatch'
    { targetViolationReasons =
        Core.Nothing,
      reference = Core.Nothing
    }

-- | The violation reason.
partialMatch_targetViolationReasons :: Lens.Lens' PartialMatch (Core.Maybe [Core.Text])
partialMatch_targetViolationReasons = Lens.lens (\PartialMatch' {targetViolationReasons} -> targetViolationReasons) (\s@PartialMatch' {} a -> s {targetViolationReasons = a} :: PartialMatch) Core.. Lens.mapping Lens._Coerce

-- | The reference rule from the master security group of the AWS Firewall
-- Manager policy.
partialMatch_reference :: Lens.Lens' PartialMatch (Core.Maybe Core.Text)
partialMatch_reference = Lens.lens (\PartialMatch' {reference} -> reference) (\s@PartialMatch' {} a -> s {reference = a} :: PartialMatch)

instance Core.FromJSON PartialMatch where
  parseJSON =
    Core.withObject
      "PartialMatch"
      ( \x ->
          PartialMatch'
            Core.<$> ( x Core..:? "TargetViolationReasons"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Reference")
      )

instance Core.Hashable PartialMatch

instance Core.NFData PartialMatch
