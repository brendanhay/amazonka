{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.SubResourceType
import Network.AWS.Shield.Types.SummarizedAttackVector
import Network.AWS.Shield.Types.SummarizedCounter

-- | The attack information for the specified SubResource.
--
-- /See:/ 'newSubResourceSummary' smart constructor.
data SubResourceSummary = SubResourceSummary'
  { -- | The counters that describe the details of the attack.
    counters :: Prelude.Maybe [SummarizedCounter],
    -- | The unique identifier (ID) of the @SubResource@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The @SubResource@ type.
    type' :: Prelude.Maybe SubResourceType,
    -- | The list of attack types and associated counters.
    attackVectors :: Prelude.Maybe [SummarizedAttackVector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { counters = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      attackVectors = Prelude.Nothing
    }

-- | The counters that describe the details of the attack.
subResourceSummary_counters :: Lens.Lens' SubResourceSummary (Prelude.Maybe [SummarizedCounter])
subResourceSummary_counters = Lens.lens (\SubResourceSummary' {counters} -> counters) (\s@SubResourceSummary' {} a -> s {counters = a} :: SubResourceSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique identifier (ID) of the @SubResource@.
subResourceSummary_id :: Lens.Lens' SubResourceSummary (Prelude.Maybe Prelude.Text)
subResourceSummary_id = Lens.lens (\SubResourceSummary' {id} -> id) (\s@SubResourceSummary' {} a -> s {id = a} :: SubResourceSummary)

-- | The @SubResource@ type.
subResourceSummary_type :: Lens.Lens' SubResourceSummary (Prelude.Maybe SubResourceType)
subResourceSummary_type = Lens.lens (\SubResourceSummary' {type'} -> type') (\s@SubResourceSummary' {} a -> s {type' = a} :: SubResourceSummary)

-- | The list of attack types and associated counters.
subResourceSummary_attackVectors :: Lens.Lens' SubResourceSummary (Prelude.Maybe [SummarizedAttackVector])
subResourceSummary_attackVectors = Lens.lens (\SubResourceSummary' {attackVectors} -> attackVectors) (\s@SubResourceSummary' {} a -> s {attackVectors = a} :: SubResourceSummary) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON SubResourceSummary where
  parseJSON =
    Prelude.withObject
      "SubResourceSummary"
      ( \x ->
          SubResourceSummary'
            Prelude.<$> (x Prelude..:? "Counters" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> ( x Prelude..:? "AttackVectors"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SubResourceSummary

instance Prelude.NFData SubResourceSummary
