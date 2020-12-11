{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.DisassociateDiscoveredResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate an Application Discovery Service discovered resource from a migration task.
module Network.AWS.MigrationHub.DisassociateDiscoveredResource
  ( -- * Creating a request
    DisassociateDiscoveredResource (..),
    mkDisassociateDiscoveredResource,

    -- ** Request lenses
    ddrDryRun,
    ddrProgressUpdateStream,
    ddrMigrationTaskName,
    ddrConfigurationId,

    -- * Destructuring the response
    DisassociateDiscoveredResourceResponse (..),
    mkDisassociateDiscoveredResourceResponse,

    -- ** Response lenses
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateDiscoveredResource' smart constructor.
data DisassociateDiscoveredResource = DisassociateDiscoveredResource'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    progressUpdateStream ::
      Lude.Text,
    migrationTaskName ::
      Lude.Text,
    configurationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDiscoveredResource' with the minimum fields required to make a request.
--
-- * 'configurationId' - ConfigurationId of the Application Discovery Service resource to be disassociated.
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
-- * 'migrationTaskName' - The identifier given to the MigrationTask. /Do not store personal data in this field./
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
mkDisassociateDiscoveredResource ::
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  -- | 'configurationId'
  Lude.Text ->
  DisassociateDiscoveredResource
mkDisassociateDiscoveredResource
  pProgressUpdateStream_
  pMigrationTaskName_
  pConfigurationId_ =
    DisassociateDiscoveredResource'
      { dryRun = Lude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        configurationId = pConfigurationId_
      }

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrDryRun :: Lens.Lens' DisassociateDiscoveredResource (Lude.Maybe Lude.Bool)
ddrDryRun = Lens.lens (dryRun :: DisassociateDiscoveredResource -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisassociateDiscoveredResource)
{-# DEPRECATED ddrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrProgressUpdateStream :: Lens.Lens' DisassociateDiscoveredResource Lude.Text
ddrProgressUpdateStream = Lens.lens (progressUpdateStream :: DisassociateDiscoveredResource -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: DisassociateDiscoveredResource)
{-# DEPRECATED ddrProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | The identifier given to the MigrationTask. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrMigrationTaskName :: Lens.Lens' DisassociateDiscoveredResource Lude.Text
ddrMigrationTaskName = Lens.lens (migrationTaskName :: DisassociateDiscoveredResource -> Lude.Text) (\s a -> s {migrationTaskName = a} :: DisassociateDiscoveredResource)
{-# DEPRECATED ddrMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | ConfigurationId of the Application Discovery Service resource to be disassociated.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrConfigurationId :: Lens.Lens' DisassociateDiscoveredResource Lude.Text
ddrConfigurationId = Lens.lens (configurationId :: DisassociateDiscoveredResource -> Lude.Text) (\s a -> s {configurationId = a} :: DisassociateDiscoveredResource)
{-# DEPRECATED ddrConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

instance Lude.AWSRequest DisassociateDiscoveredResource where
  type
    Rs DisassociateDiscoveredResource =
      DisassociateDiscoveredResourceResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateDiscoveredResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateDiscoveredResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSMigrationHub.DisassociateDiscoveredResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateDiscoveredResource where
  toJSON DisassociateDiscoveredResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DryRun" Lude..=) Lude.<$> dryRun,
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName),
            Lude.Just ("ConfigurationId" Lude..= configurationId)
          ]
      )

instance Lude.ToPath DisassociateDiscoveredResource where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateDiscoveredResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateDiscoveredResourceResponse' smart constructor.
newtype DisassociateDiscoveredResourceResponse = DisassociateDiscoveredResourceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDiscoveredResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateDiscoveredResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateDiscoveredResourceResponse
mkDisassociateDiscoveredResourceResponse pResponseStatus_ =
  DisassociateDiscoveredResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DisassociateDiscoveredResourceResponse Lude.Int
ddrrsResponseStatus = Lens.lens (responseStatus :: DisassociateDiscoveredResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateDiscoveredResourceResponse)
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
