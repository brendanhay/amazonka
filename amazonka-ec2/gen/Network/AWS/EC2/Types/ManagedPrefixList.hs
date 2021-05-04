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
-- Module      : Network.AWS.EC2.Types.ManagedPrefixList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ManagedPrefixList where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PrefixListState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a managed prefix list.
--
-- /See:/ 'newManagedPrefixList' smart constructor.
data ManagedPrefixList = ManagedPrefixList'
  { -- | The state message.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the owner of the prefix list.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of entries for the prefix list.
    maxEntries :: Prelude.Maybe Prelude.Int,
    -- | The name of the prefix list.
    prefixListName :: Prelude.Maybe Prelude.Text,
    -- | The version of the prefix list.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the prefix list.
    prefixListArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the prefix list.
    state :: Prelude.Maybe PrefixListState,
    -- | The tags for the prefix list.
    tags :: Prelude.Maybe [Tag],
    -- | The IP address version.
    addressFamily :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { stateMessage = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      maxEntries = Prelude.Nothing,
      prefixListName = Prelude.Nothing,
      version = Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      prefixListArn = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      addressFamily = Prelude.Nothing
    }

-- | The state message.
managedPrefixList_stateMessage :: Lens.Lens' ManagedPrefixList (Prelude.Maybe Prelude.Text)
managedPrefixList_stateMessage = Lens.lens (\ManagedPrefixList' {stateMessage} -> stateMessage) (\s@ManagedPrefixList' {} a -> s {stateMessage = a} :: ManagedPrefixList)

-- | The ID of the owner of the prefix list.
managedPrefixList_ownerId :: Lens.Lens' ManagedPrefixList (Prelude.Maybe Prelude.Text)
managedPrefixList_ownerId = Lens.lens (\ManagedPrefixList' {ownerId} -> ownerId) (\s@ManagedPrefixList' {} a -> s {ownerId = a} :: ManagedPrefixList)

-- | The maximum number of entries for the prefix list.
managedPrefixList_maxEntries :: Lens.Lens' ManagedPrefixList (Prelude.Maybe Prelude.Int)
managedPrefixList_maxEntries = Lens.lens (\ManagedPrefixList' {maxEntries} -> maxEntries) (\s@ManagedPrefixList' {} a -> s {maxEntries = a} :: ManagedPrefixList)

-- | The name of the prefix list.
managedPrefixList_prefixListName :: Lens.Lens' ManagedPrefixList (Prelude.Maybe Prelude.Text)
managedPrefixList_prefixListName = Lens.lens (\ManagedPrefixList' {prefixListName} -> prefixListName) (\s@ManagedPrefixList' {} a -> s {prefixListName = a} :: ManagedPrefixList)

-- | The version of the prefix list.
managedPrefixList_version :: Lens.Lens' ManagedPrefixList (Prelude.Maybe Prelude.Integer)
managedPrefixList_version = Lens.lens (\ManagedPrefixList' {version} -> version) (\s@ManagedPrefixList' {} a -> s {version = a} :: ManagedPrefixList)

-- | The ID of the prefix list.
managedPrefixList_prefixListId :: Lens.Lens' ManagedPrefixList (Prelude.Maybe Prelude.Text)
managedPrefixList_prefixListId = Lens.lens (\ManagedPrefixList' {prefixListId} -> prefixListId) (\s@ManagedPrefixList' {} a -> s {prefixListId = a} :: ManagedPrefixList)

-- | The Amazon Resource Name (ARN) for the prefix list.
managedPrefixList_prefixListArn :: Lens.Lens' ManagedPrefixList (Prelude.Maybe Prelude.Text)
managedPrefixList_prefixListArn = Lens.lens (\ManagedPrefixList' {prefixListArn} -> prefixListArn) (\s@ManagedPrefixList' {} a -> s {prefixListArn = a} :: ManagedPrefixList)

-- | The state of the prefix list.
managedPrefixList_state :: Lens.Lens' ManagedPrefixList (Prelude.Maybe PrefixListState)
managedPrefixList_state = Lens.lens (\ManagedPrefixList' {state} -> state) (\s@ManagedPrefixList' {} a -> s {state = a} :: ManagedPrefixList)

-- | The tags for the prefix list.
managedPrefixList_tags :: Lens.Lens' ManagedPrefixList (Prelude.Maybe [Tag])
managedPrefixList_tags = Lens.lens (\ManagedPrefixList' {tags} -> tags) (\s@ManagedPrefixList' {} a -> s {tags = a} :: ManagedPrefixList) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP address version.
managedPrefixList_addressFamily :: Lens.Lens' ManagedPrefixList (Prelude.Maybe Prelude.Text)
managedPrefixList_addressFamily = Lens.lens (\ManagedPrefixList' {addressFamily} -> addressFamily) (\s@ManagedPrefixList' {} a -> s {addressFamily = a} :: ManagedPrefixList)

instance Prelude.FromXML ManagedPrefixList where
  parseXML x =
    ManagedPrefixList'
      Prelude.<$> (x Prelude..@? "stateMessage")
      Prelude.<*> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "maxEntries")
      Prelude.<*> (x Prelude..@? "prefixListName")
      Prelude.<*> (x Prelude..@? "version")
      Prelude.<*> (x Prelude..@? "prefixListId")
      Prelude.<*> (x Prelude..@? "prefixListArn")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "addressFamily")

instance Prelude.Hashable ManagedPrefixList

instance Prelude.NFData ManagedPrefixList
