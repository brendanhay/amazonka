{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed prefix list. You must first remove all references to the prefix list in your resources.
module Network.AWS.EC2.DeleteManagedPrefixList
  ( -- * Creating a request
    DeleteManagedPrefixList (..),
    mkDeleteManagedPrefixList,

    -- ** Request lenses
    dmplDryRun,
    dmplPrefixListId,

    -- * Destructuring the response
    DeleteManagedPrefixListResponse (..),
    mkDeleteManagedPrefixListResponse,

    -- ** Response lenses
    dmplrsPrefixList,
    dmplrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteManagedPrefixList' smart constructor.
data DeleteManagedPrefixList = DeleteManagedPrefixList'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    prefixListId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteManagedPrefixList' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'prefixListId' - The ID of the prefix list.
mkDeleteManagedPrefixList ::
  -- | 'prefixListId'
  Lude.Text ->
  DeleteManagedPrefixList
mkDeleteManagedPrefixList pPrefixListId_ =
  DeleteManagedPrefixList'
    { dryRun = Lude.Nothing,
      prefixListId = pPrefixListId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplDryRun :: Lens.Lens' DeleteManagedPrefixList (Lude.Maybe Lude.Bool)
dmplDryRun = Lens.lens (dryRun :: DeleteManagedPrefixList -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteManagedPrefixList)
{-# DEPRECATED dmplDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplPrefixListId :: Lens.Lens' DeleteManagedPrefixList Lude.Text
dmplPrefixListId = Lens.lens (prefixListId :: DeleteManagedPrefixList -> Lude.Text) (\s a -> s {prefixListId = a} :: DeleteManagedPrefixList)
{-# DEPRECATED dmplPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

instance Lude.AWSRequest DeleteManagedPrefixList where
  type Rs DeleteManagedPrefixList = DeleteManagedPrefixListResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteManagedPrefixListResponse'
            Lude.<$> (x Lude..@? "prefixList") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteManagedPrefixList where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteManagedPrefixList where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteManagedPrefixList where
  toQuery DeleteManagedPrefixList' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteManagedPrefixList" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "PrefixListId" Lude.=: prefixListId
      ]

-- | /See:/ 'mkDeleteManagedPrefixListResponse' smart constructor.
data DeleteManagedPrefixListResponse = DeleteManagedPrefixListResponse'
  { prefixList ::
      Lude.Maybe
        ManagedPrefixList,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteManagedPrefixListResponse' with the minimum fields required to make a request.
--
-- * 'prefixList' - Information about the prefix list.
-- * 'responseStatus' - The response status code.
mkDeleteManagedPrefixListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteManagedPrefixListResponse
mkDeleteManagedPrefixListResponse pResponseStatus_ =
  DeleteManagedPrefixListResponse'
    { prefixList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrsPrefixList :: Lens.Lens' DeleteManagedPrefixListResponse (Lude.Maybe ManagedPrefixList)
dmplrsPrefixList = Lens.lens (prefixList :: DeleteManagedPrefixListResponse -> Lude.Maybe ManagedPrefixList) (\s a -> s {prefixList = a} :: DeleteManagedPrefixListResponse)
{-# DEPRECATED dmplrsPrefixList "Use generic-lens or generic-optics with 'prefixList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrsResponseStatus :: Lens.Lens' DeleteManagedPrefixListResponse Lude.Int
dmplrsResponseStatus = Lens.lens (responseStatus :: DeleteManagedPrefixListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteManagedPrefixListResponse)
{-# DEPRECATED dmplrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
