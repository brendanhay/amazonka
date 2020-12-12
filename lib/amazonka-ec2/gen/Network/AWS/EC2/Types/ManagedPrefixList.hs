{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ManagedPrefixList
  ( ManagedPrefixList (..),

    -- * Smart constructor
    mkManagedPrefixList,

    -- * Lenses
    mplStateMessage,
    mplState,
    mplPrefixListARN,
    mplAddressFamily,
    mplOwnerId,
    mplPrefixListId,
    mplVersion,
    mplPrefixListName,
    mplMaxEntries,
    mplTags,
  )
where

import Network.AWS.EC2.Types.PrefixListState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a managed prefix list.
--
-- /See:/ 'mkManagedPrefixList' smart constructor.
data ManagedPrefixList = ManagedPrefixList'
  { stateMessage ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe PrefixListState,
    prefixListARN :: Lude.Maybe Lude.Text,
    addressFamily :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    prefixListId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Integer,
    prefixListName :: Lude.Maybe Lude.Text,
    maxEntries :: Lude.Maybe Lude.Int,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ManagedPrefixList' with the minimum fields required to make a request.
--
-- * 'addressFamily' - The IP address version.
-- * 'maxEntries' - The maximum number of entries for the prefix list.
-- * 'ownerId' - The ID of the owner of the prefix list.
-- * 'prefixListARN' - The Amazon Resource Name (ARN) for the prefix list.
-- * 'prefixListId' - The ID of the prefix list.
-- * 'prefixListName' - The name of the prefix list.
-- * 'state' - The state of the prefix list.
-- * 'stateMessage' - The state message.
-- * 'tags' - The tags for the prefix list.
-- * 'version' - The version of the prefix list.
mkManagedPrefixList ::
  ManagedPrefixList
mkManagedPrefixList =
  ManagedPrefixList'
    { stateMessage = Lude.Nothing,
      state = Lude.Nothing,
      prefixListARN = Lude.Nothing,
      addressFamily = Lude.Nothing,
      ownerId = Lude.Nothing,
      prefixListId = Lude.Nothing,
      version = Lude.Nothing,
      prefixListName = Lude.Nothing,
      maxEntries = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The state message.
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplStateMessage :: Lens.Lens' ManagedPrefixList (Lude.Maybe Lude.Text)
mplStateMessage = Lens.lens (stateMessage :: ManagedPrefixList -> Lude.Maybe Lude.Text) (\s a -> s {stateMessage = a} :: ManagedPrefixList)
{-# DEPRECATED mplStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | The state of the prefix list.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplState :: Lens.Lens' ManagedPrefixList (Lude.Maybe PrefixListState)
mplState = Lens.lens (state :: ManagedPrefixList -> Lude.Maybe PrefixListState) (\s a -> s {state = a} :: ManagedPrefixList)
{-# DEPRECATED mplState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) for the prefix list.
--
-- /Note:/ Consider using 'prefixListARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplPrefixListARN :: Lens.Lens' ManagedPrefixList (Lude.Maybe Lude.Text)
mplPrefixListARN = Lens.lens (prefixListARN :: ManagedPrefixList -> Lude.Maybe Lude.Text) (\s a -> s {prefixListARN = a} :: ManagedPrefixList)
{-# DEPRECATED mplPrefixListARN "Use generic-lens or generic-optics with 'prefixListARN' instead." #-}

-- | The IP address version.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplAddressFamily :: Lens.Lens' ManagedPrefixList (Lude.Maybe Lude.Text)
mplAddressFamily = Lens.lens (addressFamily :: ManagedPrefixList -> Lude.Maybe Lude.Text) (\s a -> s {addressFamily = a} :: ManagedPrefixList)
{-# DEPRECATED mplAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The ID of the owner of the prefix list.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplOwnerId :: Lens.Lens' ManagedPrefixList (Lude.Maybe Lude.Text)
mplOwnerId = Lens.lens (ownerId :: ManagedPrefixList -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: ManagedPrefixList)
{-# DEPRECATED mplOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplPrefixListId :: Lens.Lens' ManagedPrefixList (Lude.Maybe Lude.Text)
mplPrefixListId = Lens.lens (prefixListId :: ManagedPrefixList -> Lude.Maybe Lude.Text) (\s a -> s {prefixListId = a} :: ManagedPrefixList)
{-# DEPRECATED mplPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | The version of the prefix list.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplVersion :: Lens.Lens' ManagedPrefixList (Lude.Maybe Lude.Integer)
mplVersion = Lens.lens (version :: ManagedPrefixList -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: ManagedPrefixList)
{-# DEPRECATED mplVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the prefix list.
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplPrefixListName :: Lens.Lens' ManagedPrefixList (Lude.Maybe Lude.Text)
mplPrefixListName = Lens.lens (prefixListName :: ManagedPrefixList -> Lude.Maybe Lude.Text) (\s a -> s {prefixListName = a} :: ManagedPrefixList)
{-# DEPRECATED mplPrefixListName "Use generic-lens or generic-optics with 'prefixListName' instead." #-}

-- | The maximum number of entries for the prefix list.
--
-- /Note:/ Consider using 'maxEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplMaxEntries :: Lens.Lens' ManagedPrefixList (Lude.Maybe Lude.Int)
mplMaxEntries = Lens.lens (maxEntries :: ManagedPrefixList -> Lude.Maybe Lude.Int) (\s a -> s {maxEntries = a} :: ManagedPrefixList)
{-# DEPRECATED mplMaxEntries "Use generic-lens or generic-optics with 'maxEntries' instead." #-}

-- | The tags for the prefix list.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplTags :: Lens.Lens' ManagedPrefixList (Lude.Maybe [Tag])
mplTags = Lens.lens (tags :: ManagedPrefixList -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ManagedPrefixList)
{-# DEPRECATED mplTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ManagedPrefixList where
  parseXML x =
    ManagedPrefixList'
      Lude.<$> (x Lude..@? "stateMessage")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "prefixListArn")
      Lude.<*> (x Lude..@? "addressFamily")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "prefixListId")
      Lude.<*> (x Lude..@? "version")
      Lude.<*> (x Lude..@? "prefixListName")
      Lude.<*> (x Lude..@? "maxEntries")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
