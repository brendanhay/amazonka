{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RestoreManagedPrefixListVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the entries from a previous version of a managed prefix list to a new version of the prefix list.
module Network.AWS.EC2.RestoreManagedPrefixListVersion
  ( -- * Creating a request
    RestoreManagedPrefixListVersion (..),
    mkRestoreManagedPrefixListVersion,

    -- ** Request lenses
    rmplvCurrentVersion,
    rmplvPrefixListId,
    rmplvPreviousVersion,
    rmplvDryRun,

    -- * Destructuring the response
    RestoreManagedPrefixListVersionResponse (..),
    mkRestoreManagedPrefixListVersionResponse,

    -- ** Response lenses
    rmplvrsPrefixList,
    rmplvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreManagedPrefixListVersion' smart constructor.
data RestoreManagedPrefixListVersion = RestoreManagedPrefixListVersion'
  { -- | The current version number for the prefix list.
    currentVersion :: Lude.Integer,
    -- | The ID of the prefix list.
    prefixListId :: Lude.Text,
    -- | The version to restore.
    previousVersion :: Lude.Integer,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreManagedPrefixListVersion' with the minimum fields required to make a request.
--
-- * 'currentVersion' - The current version number for the prefix list.
-- * 'prefixListId' - The ID of the prefix list.
-- * 'previousVersion' - The version to restore.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRestoreManagedPrefixListVersion ::
  -- | 'currentVersion'
  Lude.Integer ->
  -- | 'prefixListId'
  Lude.Text ->
  -- | 'previousVersion'
  Lude.Integer ->
  RestoreManagedPrefixListVersion
mkRestoreManagedPrefixListVersion
  pCurrentVersion_
  pPrefixListId_
  pPreviousVersion_ =
    RestoreManagedPrefixListVersion'
      { currentVersion =
          pCurrentVersion_,
        prefixListId = pPrefixListId_,
        previousVersion = pPreviousVersion_,
        dryRun = Lude.Nothing
      }

-- | The current version number for the prefix list.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvCurrentVersion :: Lens.Lens' RestoreManagedPrefixListVersion Lude.Integer
rmplvCurrentVersion = Lens.lens (currentVersion :: RestoreManagedPrefixListVersion -> Lude.Integer) (\s a -> s {currentVersion = a} :: RestoreManagedPrefixListVersion)
{-# DEPRECATED rmplvCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvPrefixListId :: Lens.Lens' RestoreManagedPrefixListVersion Lude.Text
rmplvPrefixListId = Lens.lens (prefixListId :: RestoreManagedPrefixListVersion -> Lude.Text) (\s a -> s {prefixListId = a} :: RestoreManagedPrefixListVersion)
{-# DEPRECATED rmplvPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | The version to restore.
--
-- /Note:/ Consider using 'previousVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvPreviousVersion :: Lens.Lens' RestoreManagedPrefixListVersion Lude.Integer
rmplvPreviousVersion = Lens.lens (previousVersion :: RestoreManagedPrefixListVersion -> Lude.Integer) (\s a -> s {previousVersion = a} :: RestoreManagedPrefixListVersion)
{-# DEPRECATED rmplvPreviousVersion "Use generic-lens or generic-optics with 'previousVersion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvDryRun :: Lens.Lens' RestoreManagedPrefixListVersion (Lude.Maybe Lude.Bool)
rmplvDryRun = Lens.lens (dryRun :: RestoreManagedPrefixListVersion -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RestoreManagedPrefixListVersion)
{-# DEPRECATED rmplvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest RestoreManagedPrefixListVersion where
  type
    Rs RestoreManagedPrefixListVersion =
      RestoreManagedPrefixListVersionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RestoreManagedPrefixListVersionResponse'
            Lude.<$> (x Lude..@? "prefixList") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreManagedPrefixListVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestoreManagedPrefixListVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreManagedPrefixListVersion where
  toQuery RestoreManagedPrefixListVersion' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RestoreManagedPrefixListVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CurrentVersion" Lude.=: currentVersion,
        "PrefixListId" Lude.=: prefixListId,
        "PreviousVersion" Lude.=: previousVersion,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRestoreManagedPrefixListVersionResponse' smart constructor.
data RestoreManagedPrefixListVersionResponse = RestoreManagedPrefixListVersionResponse'
  { -- | Information about the prefix list.
    prefixList :: Lude.Maybe ManagedPrefixList,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreManagedPrefixListVersionResponse' with the minimum fields required to make a request.
--
-- * 'prefixList' - Information about the prefix list.
-- * 'responseStatus' - The response status code.
mkRestoreManagedPrefixListVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreManagedPrefixListVersionResponse
mkRestoreManagedPrefixListVersionResponse pResponseStatus_ =
  RestoreManagedPrefixListVersionResponse'
    { prefixList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvrsPrefixList :: Lens.Lens' RestoreManagedPrefixListVersionResponse (Lude.Maybe ManagedPrefixList)
rmplvrsPrefixList = Lens.lens (prefixList :: RestoreManagedPrefixListVersionResponse -> Lude.Maybe ManagedPrefixList) (\s a -> s {prefixList = a} :: RestoreManagedPrefixListVersionResponse)
{-# DEPRECATED rmplvrsPrefixList "Use generic-lens or generic-optics with 'prefixList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmplvrsResponseStatus :: Lens.Lens' RestoreManagedPrefixListVersionResponse Lude.Int
rmplvrsResponseStatus = Lens.lens (responseStatus :: RestoreManagedPrefixListVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreManagedPrefixListVersionResponse)
{-# DEPRECATED rmplvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
