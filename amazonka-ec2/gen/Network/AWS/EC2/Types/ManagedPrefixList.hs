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
-- Module      : Network.AWS.EC2.Types.ManagedPrefixList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ManagedPrefixList where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PrefixListState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a managed prefix list.
--
-- /See:/ 'newManagedPrefixList' smart constructor.
data ManagedPrefixList = ManagedPrefixList'
  { -- | The state message.
    stateMessage :: Core.Maybe Core.Text,
    -- | The ID of the owner of the prefix list.
    ownerId :: Core.Maybe Core.Text,
    -- | The maximum number of entries for the prefix list.
    maxEntries :: Core.Maybe Core.Int,
    -- | The name of the prefix list.
    prefixListName :: Core.Maybe Core.Text,
    -- | The version of the prefix list.
    version :: Core.Maybe Core.Integer,
    -- | The ID of the prefix list.
    prefixListId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the prefix list.
    prefixListArn :: Core.Maybe Core.Text,
    -- | The state of the prefix list.
    state :: Core.Maybe PrefixListState,
    -- | The tags for the prefix list.
    tags :: Core.Maybe [Tag],
    -- | The IP address version.
    addressFamily :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ManagedPrefixList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMessage', 'managedPrefixList_stateMessage' - The state message.
--
-- 'ownerId', 'managedPrefixList_ownerId' - The ID of the owner of the prefix list.
--
-- 'maxEntries', 'managedPrefixList_maxEntries' - The maximum number of entries for the prefix list.
--
-- 'prefixListName', 'managedPrefixList_prefixListName' - The name of the prefix list.
--
-- 'version', 'managedPrefixList_version' - The version of the prefix list.
--
-- 'prefixListId', 'managedPrefixList_prefixListId' - The ID of the prefix list.
--
-- 'prefixListArn', 'managedPrefixList_prefixListArn' - The Amazon Resource Name (ARN) for the prefix list.
--
-- 'state', 'managedPrefixList_state' - The state of the prefix list.
--
-- 'tags', 'managedPrefixList_tags' - The tags for the prefix list.
--
-- 'addressFamily', 'managedPrefixList_addressFamily' - The IP address version.
newManagedPrefixList ::
  ManagedPrefixList
newManagedPrefixList =
  ManagedPrefixList'
    { stateMessage = Core.Nothing,
      ownerId = Core.Nothing,
      maxEntries = Core.Nothing,
      prefixListName = Core.Nothing,
      version = Core.Nothing,
      prefixListId = Core.Nothing,
      prefixListArn = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      addressFamily = Core.Nothing
    }

-- | The state message.
managedPrefixList_stateMessage :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
managedPrefixList_stateMessage = Lens.lens (\ManagedPrefixList' {stateMessage} -> stateMessage) (\s@ManagedPrefixList' {} a -> s {stateMessage = a} :: ManagedPrefixList)

-- | The ID of the owner of the prefix list.
managedPrefixList_ownerId :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
managedPrefixList_ownerId = Lens.lens (\ManagedPrefixList' {ownerId} -> ownerId) (\s@ManagedPrefixList' {} a -> s {ownerId = a} :: ManagedPrefixList)

-- | The maximum number of entries for the prefix list.
managedPrefixList_maxEntries :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Int)
managedPrefixList_maxEntries = Lens.lens (\ManagedPrefixList' {maxEntries} -> maxEntries) (\s@ManagedPrefixList' {} a -> s {maxEntries = a} :: ManagedPrefixList)

-- | The name of the prefix list.
managedPrefixList_prefixListName :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
managedPrefixList_prefixListName = Lens.lens (\ManagedPrefixList' {prefixListName} -> prefixListName) (\s@ManagedPrefixList' {} a -> s {prefixListName = a} :: ManagedPrefixList)

-- | The version of the prefix list.
managedPrefixList_version :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Integer)
managedPrefixList_version = Lens.lens (\ManagedPrefixList' {version} -> version) (\s@ManagedPrefixList' {} a -> s {version = a} :: ManagedPrefixList)

-- | The ID of the prefix list.
managedPrefixList_prefixListId :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
managedPrefixList_prefixListId = Lens.lens (\ManagedPrefixList' {prefixListId} -> prefixListId) (\s@ManagedPrefixList' {} a -> s {prefixListId = a} :: ManagedPrefixList)

-- | The Amazon Resource Name (ARN) for the prefix list.
managedPrefixList_prefixListArn :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
managedPrefixList_prefixListArn = Lens.lens (\ManagedPrefixList' {prefixListArn} -> prefixListArn) (\s@ManagedPrefixList' {} a -> s {prefixListArn = a} :: ManagedPrefixList)

-- | The state of the prefix list.
managedPrefixList_state :: Lens.Lens' ManagedPrefixList (Core.Maybe PrefixListState)
managedPrefixList_state = Lens.lens (\ManagedPrefixList' {state} -> state) (\s@ManagedPrefixList' {} a -> s {state = a} :: ManagedPrefixList)

-- | The tags for the prefix list.
managedPrefixList_tags :: Lens.Lens' ManagedPrefixList (Core.Maybe [Tag])
managedPrefixList_tags = Lens.lens (\ManagedPrefixList' {tags} -> tags) (\s@ManagedPrefixList' {} a -> s {tags = a} :: ManagedPrefixList) Core.. Lens.mapping Lens._Coerce

-- | The IP address version.
managedPrefixList_addressFamily :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
managedPrefixList_addressFamily = Lens.lens (\ManagedPrefixList' {addressFamily} -> addressFamily) (\s@ManagedPrefixList' {} a -> s {addressFamily = a} :: ManagedPrefixList)

instance Core.FromXML ManagedPrefixList where
  parseXML x =
    ManagedPrefixList'
      Core.<$> (x Core..@? "stateMessage")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "maxEntries")
      Core.<*> (x Core..@? "prefixListName")
      Core.<*> (x Core..@? "version")
      Core.<*> (x Core..@? "prefixListId")
      Core.<*> (x Core..@? "prefixListArn")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "addressFamily")

instance Core.Hashable ManagedPrefixList

instance Core.NFData ManagedPrefixList
