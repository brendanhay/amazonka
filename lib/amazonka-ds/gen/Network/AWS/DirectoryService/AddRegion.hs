{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AddRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds two domain controllers in the specified Region for the specified directory.
module Network.AWS.DirectoryService.AddRegion
  ( -- * Creating a request
    AddRegion (..),
    mkAddRegion,

    -- ** Request lenses
    arDirectoryId,
    arRegionName,
    arVPCSettings,

    -- * Destructuring the response
    AddRegionResponse (..),
    mkAddRegionResponse,

    -- ** Response lenses
    arrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddRegion' smart constructor.
data AddRegion = AddRegion'
  { directoryId :: Lude.Text,
    regionName :: Lude.Text,
    vpcSettings :: DirectoryVPCSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddRegion' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory to which you want to add Region replication.
-- * 'regionName' - The name of the Region where you want to add domain controllers for replication. For example, @us-east-1@ .
-- * 'vpcSettings' - Undocumented field.
mkAddRegion ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'regionName'
  Lude.Text ->
  -- | 'vpcSettings'
  DirectoryVPCSettings ->
  AddRegion
mkAddRegion pDirectoryId_ pRegionName_ pVPCSettings_ =
  AddRegion'
    { directoryId = pDirectoryId_,
      regionName = pRegionName_,
      vpcSettings = pVPCSettings_
    }

-- | The identifier of the directory to which you want to add Region replication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDirectoryId :: Lens.Lens' AddRegion Lude.Text
arDirectoryId = Lens.lens (directoryId :: AddRegion -> Lude.Text) (\s a -> s {directoryId = a} :: AddRegion)
{-# DEPRECATED arDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the Region where you want to add domain controllers for replication. For example, @us-east-1@ .
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRegionName :: Lens.Lens' AddRegion Lude.Text
arRegionName = Lens.lens (regionName :: AddRegion -> Lude.Text) (\s a -> s {regionName = a} :: AddRegion)
{-# DEPRECATED arRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arVPCSettings :: Lens.Lens' AddRegion DirectoryVPCSettings
arVPCSettings = Lens.lens (vpcSettings :: AddRegion -> DirectoryVPCSettings) (\s a -> s {vpcSettings = a} :: AddRegion)
{-# DEPRECATED arVPCSettings "Use generic-lens or generic-optics with 'vpcSettings' instead." #-}

instance Lude.AWSRequest AddRegion where
  type Rs AddRegion = AddRegionResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddRegionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddRegion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.AddRegion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddRegion where
  toJSON AddRegion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("RegionName" Lude..= regionName),
            Lude.Just ("VPCSettings" Lude..= vpcSettings)
          ]
      )

instance Lude.ToPath AddRegion where
  toPath = Lude.const "/"

instance Lude.ToQuery AddRegion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddRegionResponse' smart constructor.
newtype AddRegionResponse = AddRegionResponse'
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

-- | Creates a value of 'AddRegionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddRegionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddRegionResponse
mkAddRegionResponse pResponseStatus_ =
  AddRegionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arrsResponseStatus :: Lens.Lens' AddRegionResponse Lude.Int
arrsResponseStatus = Lens.lens (responseStatus :: AddRegionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddRegionResponse)
{-# DEPRECATED arrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
