{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.AssociateDiscoveredResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a discovered resource ID from Application Discovery Service with a migration task.
module Network.AWS.MigrationHub.AssociateDiscoveredResource
  ( -- * Creating a request
    AssociateDiscoveredResource (..),
    mkAssociateDiscoveredResource,

    -- ** Request lenses
    adrDiscoveredResource,
    adrProgressUpdateStream,
    adrMigrationTaskName,
    adrDryRun,

    -- * Destructuring the response
    AssociateDiscoveredResourceResponse (..),
    mkAssociateDiscoveredResourceResponse,

    -- ** Response lenses
    adrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateDiscoveredResource' smart constructor.
data AssociateDiscoveredResource = AssociateDiscoveredResource'
  { -- | Object representing a Resource.
    discoveredResource :: DiscoveredResource,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Lude.Text,
    -- | The identifier given to the MigrationTask. /Do not store personal data in this field./
    migrationTaskName :: Lude.Text,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDiscoveredResource' with the minimum fields required to make a request.
--
-- * 'discoveredResource' - Object representing a Resource.
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
-- * 'migrationTaskName' - The identifier given to the MigrationTask. /Do not store personal data in this field./
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
mkAssociateDiscoveredResource ::
  -- | 'discoveredResource'
  DiscoveredResource ->
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  AssociateDiscoveredResource
mkAssociateDiscoveredResource
  pDiscoveredResource_
  pProgressUpdateStream_
  pMigrationTaskName_ =
    AssociateDiscoveredResource'
      { discoveredResource =
          pDiscoveredResource_,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        dryRun = Lude.Nothing
      }

-- | Object representing a Resource.
--
-- /Note:/ Consider using 'discoveredResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrDiscoveredResource :: Lens.Lens' AssociateDiscoveredResource DiscoveredResource
adrDiscoveredResource = Lens.lens (discoveredResource :: AssociateDiscoveredResource -> DiscoveredResource) (\s a -> s {discoveredResource = a} :: AssociateDiscoveredResource)
{-# DEPRECATED adrDiscoveredResource "Use generic-lens or generic-optics with 'discoveredResource' instead." #-}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrProgressUpdateStream :: Lens.Lens' AssociateDiscoveredResource Lude.Text
adrProgressUpdateStream = Lens.lens (progressUpdateStream :: AssociateDiscoveredResource -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: AssociateDiscoveredResource)
{-# DEPRECATED adrProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | The identifier given to the MigrationTask. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrMigrationTaskName :: Lens.Lens' AssociateDiscoveredResource Lude.Text
adrMigrationTaskName = Lens.lens (migrationTaskName :: AssociateDiscoveredResource -> Lude.Text) (\s a -> s {migrationTaskName = a} :: AssociateDiscoveredResource)
{-# DEPRECATED adrMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrDryRun :: Lens.Lens' AssociateDiscoveredResource (Lude.Maybe Lude.Bool)
adrDryRun = Lens.lens (dryRun :: AssociateDiscoveredResource -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateDiscoveredResource)
{-# DEPRECATED adrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AssociateDiscoveredResource where
  type
    Rs AssociateDiscoveredResource =
      AssociateDiscoveredResourceResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateDiscoveredResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateDiscoveredResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.AssociateDiscoveredResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateDiscoveredResource where
  toJSON AssociateDiscoveredResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DiscoveredResource" Lude..= discoveredResource),
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName),
            ("DryRun" Lude..=) Lude.<$> dryRun
          ]
      )

instance Lude.ToPath AssociateDiscoveredResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateDiscoveredResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateDiscoveredResourceResponse' smart constructor.
newtype AssociateDiscoveredResourceResponse = AssociateDiscoveredResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDiscoveredResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateDiscoveredResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateDiscoveredResourceResponse
mkAssociateDiscoveredResourceResponse pResponseStatus_ =
  AssociateDiscoveredResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrrsResponseStatus :: Lens.Lens' AssociateDiscoveredResourceResponse Lude.Int
adrrsResponseStatus = Lens.lens (responseStatus :: AssociateDiscoveredResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateDiscoveredResourceResponse)
{-# DEPRECATED adrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
