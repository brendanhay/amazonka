{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified managed prefix list.
--
-- Adding or removing entries in a prefix list creates a new version of the prefix list. Changing the name of the prefix list does not affect the version.
-- If you specify a current version number that does not match the true current version number, the request fails.
module Network.AWS.EC2.ModifyManagedPrefixList
  ( -- * Creating a request
    ModifyManagedPrefixList (..),
    mkModifyManagedPrefixList,

    -- ** Request lenses
    mmplCurrentVersion,
    mmplRemoveEntries,
    mmplPrefixListId,
    mmplPrefixListName,
    mmplAddEntries,
    mmplDryRun,

    -- * Destructuring the response
    ModifyManagedPrefixListResponse (..),
    mkModifyManagedPrefixListResponse,

    -- ** Response lenses
    mmplrsPrefixList,
    mmplrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyManagedPrefixList' smart constructor.
data ModifyManagedPrefixList = ModifyManagedPrefixList'
  { -- | The current version of the prefix list.
    currentVersion :: Lude.Maybe Lude.Integer,
    -- | One or more entries to remove from the prefix list.
    removeEntries :: Lude.Maybe [RemovePrefixListEntry],
    -- | The ID of the prefix list.
    prefixListId :: Lude.Text,
    -- | A name for the prefix list.
    prefixListName :: Lude.Maybe Lude.Text,
    -- | One or more entries to add to the prefix list.
    addEntries :: Lude.Maybe [AddPrefixListEntry],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyManagedPrefixList' with the minimum fields required to make a request.
--
-- * 'currentVersion' - The current version of the prefix list.
-- * 'removeEntries' - One or more entries to remove from the prefix list.
-- * 'prefixListId' - The ID of the prefix list.
-- * 'prefixListName' - A name for the prefix list.
-- * 'addEntries' - One or more entries to add to the prefix list.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyManagedPrefixList ::
  -- | 'prefixListId'
  Lude.Text ->
  ModifyManagedPrefixList
mkModifyManagedPrefixList pPrefixListId_ =
  ModifyManagedPrefixList'
    { currentVersion = Lude.Nothing,
      removeEntries = Lude.Nothing,
      prefixListId = pPrefixListId_,
      prefixListName = Lude.Nothing,
      addEntries = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The current version of the prefix list.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplCurrentVersion :: Lens.Lens' ModifyManagedPrefixList (Lude.Maybe Lude.Integer)
mmplCurrentVersion = Lens.lens (currentVersion :: ModifyManagedPrefixList -> Lude.Maybe Lude.Integer) (\s a -> s {currentVersion = a} :: ModifyManagedPrefixList)
{-# DEPRECATED mmplCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | One or more entries to remove from the prefix list.
--
-- /Note:/ Consider using 'removeEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplRemoveEntries :: Lens.Lens' ModifyManagedPrefixList (Lude.Maybe [RemovePrefixListEntry])
mmplRemoveEntries = Lens.lens (removeEntries :: ModifyManagedPrefixList -> Lude.Maybe [RemovePrefixListEntry]) (\s a -> s {removeEntries = a} :: ModifyManagedPrefixList)
{-# DEPRECATED mmplRemoveEntries "Use generic-lens or generic-optics with 'removeEntries' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplPrefixListId :: Lens.Lens' ModifyManagedPrefixList Lude.Text
mmplPrefixListId = Lens.lens (prefixListId :: ModifyManagedPrefixList -> Lude.Text) (\s a -> s {prefixListId = a} :: ModifyManagedPrefixList)
{-# DEPRECATED mmplPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | A name for the prefix list.
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplPrefixListName :: Lens.Lens' ModifyManagedPrefixList (Lude.Maybe Lude.Text)
mmplPrefixListName = Lens.lens (prefixListName :: ModifyManagedPrefixList -> Lude.Maybe Lude.Text) (\s a -> s {prefixListName = a} :: ModifyManagedPrefixList)
{-# DEPRECATED mmplPrefixListName "Use generic-lens or generic-optics with 'prefixListName' instead." #-}

-- | One or more entries to add to the prefix list.
--
-- /Note:/ Consider using 'addEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplAddEntries :: Lens.Lens' ModifyManagedPrefixList (Lude.Maybe [AddPrefixListEntry])
mmplAddEntries = Lens.lens (addEntries :: ModifyManagedPrefixList -> Lude.Maybe [AddPrefixListEntry]) (\s a -> s {addEntries = a} :: ModifyManagedPrefixList)
{-# DEPRECATED mmplAddEntries "Use generic-lens or generic-optics with 'addEntries' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplDryRun :: Lens.Lens' ModifyManagedPrefixList (Lude.Maybe Lude.Bool)
mmplDryRun = Lens.lens (dryRun :: ModifyManagedPrefixList -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyManagedPrefixList)
{-# DEPRECATED mmplDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyManagedPrefixList where
  type Rs ModifyManagedPrefixList = ModifyManagedPrefixListResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyManagedPrefixListResponse'
            Lude.<$> (x Lude..@? "prefixList") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyManagedPrefixList where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyManagedPrefixList where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyManagedPrefixList where
  toQuery ModifyManagedPrefixList' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyManagedPrefixList" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CurrentVersion" Lude.=: currentVersion,
        Lude.toQuery
          (Lude.toQueryList "RemoveEntry" Lude.<$> removeEntries),
        "PrefixListId" Lude.=: prefixListId,
        "PrefixListName" Lude.=: prefixListName,
        Lude.toQuery (Lude.toQueryList "AddEntry" Lude.<$> addEntries),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyManagedPrefixListResponse' smart constructor.
data ModifyManagedPrefixListResponse = ModifyManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Lude.Maybe ManagedPrefixList,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyManagedPrefixListResponse' with the minimum fields required to make a request.
--
-- * 'prefixList' - Information about the prefix list.
-- * 'responseStatus' - The response status code.
mkModifyManagedPrefixListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyManagedPrefixListResponse
mkModifyManagedPrefixListResponse pResponseStatus_ =
  ModifyManagedPrefixListResponse'
    { prefixList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplrsPrefixList :: Lens.Lens' ModifyManagedPrefixListResponse (Lude.Maybe ManagedPrefixList)
mmplrsPrefixList = Lens.lens (prefixList :: ModifyManagedPrefixListResponse -> Lude.Maybe ManagedPrefixList) (\s a -> s {prefixList = a} :: ModifyManagedPrefixListResponse)
{-# DEPRECATED mmplrsPrefixList "Use generic-lens or generic-optics with 'prefixList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmplrsResponseStatus :: Lens.Lens' ModifyManagedPrefixListResponse Lude.Int
mmplrsResponseStatus = Lens.lens (responseStatus :: ModifyManagedPrefixListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyManagedPrefixListResponse)
{-# DEPRECATED mmplrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
