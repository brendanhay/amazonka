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
-- Module      : Network.AWS.ResourceGroups.Types.ListGroupResourcesItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ListGroupResourcesItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    status :: Prelude.Maybe ResourceStatus,
    identifier :: Prelude.Maybe ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      identifier = Prelude.Nothing
    }

-- | A structure that contains the status of this resource\'s membership in
-- the group.
--
-- This field is present in the response only if the group is of type
-- @AWS::EC2::HostManagement@.
listGroupResourcesItem_status :: Lens.Lens' ListGroupResourcesItem (Prelude.Maybe ResourceStatus)
listGroupResourcesItem_status = Lens.lens (\ListGroupResourcesItem' {status} -> status) (\s@ListGroupResourcesItem' {} a -> s {status = a} :: ListGroupResourcesItem)

-- | Undocumented member.
listGroupResourcesItem_identifier :: Lens.Lens' ListGroupResourcesItem (Prelude.Maybe ResourceIdentifier)
listGroupResourcesItem_identifier = Lens.lens (\ListGroupResourcesItem' {identifier} -> identifier) (\s@ListGroupResourcesItem' {} a -> s {identifier = a} :: ListGroupResourcesItem)

instance Prelude.FromJSON ListGroupResourcesItem where
  parseJSON =
    Prelude.withObject
      "ListGroupResourcesItem"
      ( \x ->
          ListGroupResourcesItem'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Identifier")
      )

instance Prelude.Hashable ListGroupResourcesItem

instance Prelude.NFData ListGroupResourcesItem
