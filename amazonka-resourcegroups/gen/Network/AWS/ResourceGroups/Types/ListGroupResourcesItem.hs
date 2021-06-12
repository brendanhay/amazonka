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
-- Module      : Network.AWS.ResourceGroups.Types.ListGroupResourcesItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ListGroupResourcesItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ResourceGroups.Types.ResourceIdentifier
import Network.AWS.ResourceGroups.Types.ResourceStatus

-- | A structure returned by the ListGroupResources operation that contains
-- identity and group membership status information for one of the
-- resources in the group.
--
-- /See:/ 'newListGroupResourcesItem' smart constructor.
data ListGroupResourcesItem = ListGroupResourcesItem'
  { -- | A structure that contains the status of this resource\'s membership in
    -- the group.
    --
    -- This field is present in the response only if the group is of type
    -- @AWS::EC2::HostManagement@.
    status :: Core.Maybe ResourceStatus,
    identifier :: Core.Maybe ResourceIdentifier
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGroupResourcesItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listGroupResourcesItem_status' - A structure that contains the status of this resource\'s membership in
-- the group.
--
-- This field is present in the response only if the group is of type
-- @AWS::EC2::HostManagement@.
--
-- 'identifier', 'listGroupResourcesItem_identifier' - Undocumented member.
newListGroupResourcesItem ::
  ListGroupResourcesItem
newListGroupResourcesItem =
  ListGroupResourcesItem'
    { status = Core.Nothing,
      identifier = Core.Nothing
    }

-- | A structure that contains the status of this resource\'s membership in
-- the group.
--
-- This field is present in the response only if the group is of type
-- @AWS::EC2::HostManagement@.
listGroupResourcesItem_status :: Lens.Lens' ListGroupResourcesItem (Core.Maybe ResourceStatus)
listGroupResourcesItem_status = Lens.lens (\ListGroupResourcesItem' {status} -> status) (\s@ListGroupResourcesItem' {} a -> s {status = a} :: ListGroupResourcesItem)

-- | Undocumented member.
listGroupResourcesItem_identifier :: Lens.Lens' ListGroupResourcesItem (Core.Maybe ResourceIdentifier)
listGroupResourcesItem_identifier = Lens.lens (\ListGroupResourcesItem' {identifier} -> identifier) (\s@ListGroupResourcesItem' {} a -> s {identifier = a} :: ListGroupResourcesItem)

instance Core.FromJSON ListGroupResourcesItem where
  parseJSON =
    Core.withObject
      "ListGroupResourcesItem"
      ( \x ->
          ListGroupResourcesItem'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "Identifier")
      )

instance Core.Hashable ListGroupResourcesItem

instance Core.NFData ListGroupResourcesItem
