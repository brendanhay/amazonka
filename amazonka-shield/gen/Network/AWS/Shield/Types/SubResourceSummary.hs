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
-- Module      : Network.AWS.Shield.Types.SubResourceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SubResourceSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Shield.Types.SubResourceType
import Network.AWS.Shield.Types.SummarizedAttackVector
import Network.AWS.Shield.Types.SummarizedCounter

-- | The attack information for the specified SubResource.
--
-- /See:/ 'newSubResourceSummary' smart constructor.
data SubResourceSummary = SubResourceSummary'
  { -- | The counters that describe the details of the attack.
    counters :: Core.Maybe [SummarizedCounter],
    -- | The unique identifier (ID) of the @SubResource@.
    id :: Core.Maybe Core.Text,
    -- | The @SubResource@ type.
    type' :: Core.Maybe SubResourceType,
    -- | The list of attack types and associated counters.
    attackVectors :: Core.Maybe [SummarizedAttackVector]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubResourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'counters', 'subResourceSummary_counters' - The counters that describe the details of the attack.
--
-- 'id', 'subResourceSummary_id' - The unique identifier (ID) of the @SubResource@.
--
-- 'type'', 'subResourceSummary_type' - The @SubResource@ type.
--
-- 'attackVectors', 'subResourceSummary_attackVectors' - The list of attack types and associated counters.
newSubResourceSummary ::
  SubResourceSummary
newSubResourceSummary =
  SubResourceSummary'
    { counters = Core.Nothing,
      id = Core.Nothing,
      type' = Core.Nothing,
      attackVectors = Core.Nothing
    }

-- | The counters that describe the details of the attack.
subResourceSummary_counters :: Lens.Lens' SubResourceSummary (Core.Maybe [SummarizedCounter])
subResourceSummary_counters = Lens.lens (\SubResourceSummary' {counters} -> counters) (\s@SubResourceSummary' {} a -> s {counters = a} :: SubResourceSummary) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier (ID) of the @SubResource@.
subResourceSummary_id :: Lens.Lens' SubResourceSummary (Core.Maybe Core.Text)
subResourceSummary_id = Lens.lens (\SubResourceSummary' {id} -> id) (\s@SubResourceSummary' {} a -> s {id = a} :: SubResourceSummary)

-- | The @SubResource@ type.
subResourceSummary_type :: Lens.Lens' SubResourceSummary (Core.Maybe SubResourceType)
subResourceSummary_type = Lens.lens (\SubResourceSummary' {type'} -> type') (\s@SubResourceSummary' {} a -> s {type' = a} :: SubResourceSummary)

-- | The list of attack types and associated counters.
subResourceSummary_attackVectors :: Lens.Lens' SubResourceSummary (Core.Maybe [SummarizedAttackVector])
subResourceSummary_attackVectors = Lens.lens (\SubResourceSummary' {attackVectors} -> attackVectors) (\s@SubResourceSummary' {} a -> s {attackVectors = a} :: SubResourceSummary) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON SubResourceSummary where
  parseJSON =
    Core.withObject
      "SubResourceSummary"
      ( \x ->
          SubResourceSummary'
            Core.<$> (x Core..:? "Counters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "AttackVectors" Core..!= Core.mempty)
      )

instance Core.Hashable SubResourceSummary

instance Core.NFData SubResourceSummary
