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
-- Module      : Network.AWS.CloudFront.Types.ConflictingAliasesList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ConflictingAliasesList where

import Network.AWS.CloudFront.Types.ConflictingAlias
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of aliases (also called CNAMEs) and the CloudFront distributions
-- and Amazon Web Services accounts that they are associated with. In the
-- list, the distribution and account IDs are partially hidden, which
-- allows you to identify the distributions and accounts that you own, but
-- helps to protect the information of ones that you don’t own.
--
-- /See:/ 'newConflictingAliasesList' smart constructor.
data ConflictingAliasesList = ConflictingAliasesList'
  { -- | Contains the conflicting aliases in the list.
    items :: Prelude.Maybe [ConflictingAlias],
    -- | The number of conflicting aliases returned in the response.
    quantity :: Prelude.Maybe Prelude.Int,
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing conflicting
    -- aliases where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of conflicting aliases requested.
    maxItems :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConflictingAliasesList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'conflictingAliasesList_items' - Contains the conflicting aliases in the list.
--
-- 'quantity', 'conflictingAliasesList_quantity' - The number of conflicting aliases returned in the response.
--
-- 'nextMarker', 'conflictingAliasesList_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing conflicting
-- aliases where you left off.
--
-- 'maxItems', 'conflictingAliasesList_maxItems' - The maximum number of conflicting aliases requested.
newConflictingAliasesList ::
  ConflictingAliasesList
newConflictingAliasesList =
  ConflictingAliasesList'
    { items = Prelude.Nothing,
      quantity = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Contains the conflicting aliases in the list.
conflictingAliasesList_items :: Lens.Lens' ConflictingAliasesList (Prelude.Maybe [ConflictingAlias])
conflictingAliasesList_items = Lens.lens (\ConflictingAliasesList' {items} -> items) (\s@ConflictingAliasesList' {} a -> s {items = a} :: ConflictingAliasesList) Prelude.. Lens.mapping Lens._Coerce

-- | The number of conflicting aliases returned in the response.
conflictingAliasesList_quantity :: Lens.Lens' ConflictingAliasesList (Prelude.Maybe Prelude.Int)
conflictingAliasesList_quantity = Lens.lens (\ConflictingAliasesList' {quantity} -> quantity) (\s@ConflictingAliasesList' {} a -> s {quantity = a} :: ConflictingAliasesList)

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing conflicting
-- aliases where you left off.
conflictingAliasesList_nextMarker :: Lens.Lens' ConflictingAliasesList (Prelude.Maybe Prelude.Text)
conflictingAliasesList_nextMarker = Lens.lens (\ConflictingAliasesList' {nextMarker} -> nextMarker) (\s@ConflictingAliasesList' {} a -> s {nextMarker = a} :: ConflictingAliasesList)

-- | The maximum number of conflicting aliases requested.
conflictingAliasesList_maxItems :: Lens.Lens' ConflictingAliasesList (Prelude.Maybe Prelude.Int)
conflictingAliasesList_maxItems = Lens.lens (\ConflictingAliasesList' {maxItems} -> maxItems) (\s@ConflictingAliasesList' {} a -> s {maxItems = a} :: ConflictingAliasesList)

instance Core.FromXML ConflictingAliasesList where
  parseXML x =
    ConflictingAliasesList'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ConflictingAlias")
                  )
      Prelude.<*> (x Core..@? "Quantity")
      Prelude.<*> (x Core..@? "NextMarker")
      Prelude.<*> (x Core..@? "MaxItems")

instance Prelude.Hashable ConflictingAliasesList

instance Prelude.NFData ConflictingAliasesList
