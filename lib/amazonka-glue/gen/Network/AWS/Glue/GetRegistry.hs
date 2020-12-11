{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified registry in detail.
module Network.AWS.Glue.GetRegistry
  ( -- * Creating a request
    GetRegistry (..),
    mkGetRegistry,

    -- ** Request lenses
    grRegistryId,

    -- * Destructuring the response
    GetRegistryResponse (..),
    mkGetRegistryResponse,

    -- ** Response lenses
    grrsStatus,
    grrsRegistryName,
    grrsCreatedTime,
    grrsRegistryARN,
    grrsUpdatedTime,
    grrsDescription,
    grrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRegistry' smart constructor.
newtype GetRegistry = GetRegistry' {registryId :: RegistryId}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRegistry' with the minimum fields required to make a request.
--
-- * 'registryId' - This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
mkGetRegistry ::
  -- | 'registryId'
  RegistryId ->
  GetRegistry
mkGetRegistry pRegistryId_ =
  GetRegistry' {registryId = pRegistryId_}

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRegistryId :: Lens.Lens' GetRegistry RegistryId
grRegistryId = Lens.lens (registryId :: GetRegistry -> RegistryId) (\s a -> s {registryId = a} :: GetRegistry)
{-# DEPRECATED grRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Lude.AWSRequest GetRegistry where
  type Rs GetRegistry = GetRegistryResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRegistryResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "RegistryName")
            Lude.<*> (x Lude..?> "CreatedTime")
            Lude.<*> (x Lude..?> "RegistryArn")
            Lude.<*> (x Lude..?> "UpdatedTime")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRegistry where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetRegistry" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRegistry where
  toJSON GetRegistry' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RegistryId" Lude..= registryId)])

instance Lude.ToPath GetRegistry where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRegistry where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRegistryResponse' smart constructor.
data GetRegistryResponse = GetRegistryResponse'
  { status ::
      Lude.Maybe RegistryStatus,
    registryName :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Text,
    registryARN :: Lude.Maybe Lude.Text,
    updatedTime :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetRegistryResponse' with the minimum fields required to make a request.
--
-- * 'createdTime' - The date and time the registry was created.
-- * 'description' - A description of the registry.
-- * 'registryARN' - The Amazon Resource Name (ARN) of the registry.
-- * 'registryName' - The name of the registry.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the registry.
-- * 'updatedTime' - The date and time the registry was updated.
mkGetRegistryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRegistryResponse
mkGetRegistryResponse pResponseStatus_ =
  GetRegistryResponse'
    { status = Lude.Nothing,
      registryName = Lude.Nothing,
      createdTime = Lude.Nothing,
      registryARN = Lude.Nothing,
      updatedTime = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the registry.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsStatus :: Lens.Lens' GetRegistryResponse (Lude.Maybe RegistryStatus)
grrsStatus = Lens.lens (status :: GetRegistryResponse -> Lude.Maybe RegistryStatus) (\s a -> s {status = a} :: GetRegistryResponse)
{-# DEPRECATED grrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRegistryName :: Lens.Lens' GetRegistryResponse (Lude.Maybe Lude.Text)
grrsRegistryName = Lens.lens (registryName :: GetRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: GetRegistryResponse)
{-# DEPRECATED grrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The date and time the registry was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsCreatedTime :: Lens.Lens' GetRegistryResponse (Lude.Maybe Lude.Text)
grrsCreatedTime = Lens.lens (createdTime :: GetRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdTime = a} :: GetRegistryResponse)
{-# DEPRECATED grrsCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the registry.
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRegistryARN :: Lens.Lens' GetRegistryResponse (Lude.Maybe Lude.Text)
grrsRegistryARN = Lens.lens (registryARN :: GetRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: GetRegistryResponse)
{-# DEPRECATED grrsRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

-- | The date and time the registry was updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsUpdatedTime :: Lens.Lens' GetRegistryResponse (Lude.Maybe Lude.Text)
grrsUpdatedTime = Lens.lens (updatedTime :: GetRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {updatedTime = a} :: GetRegistryResponse)
{-# DEPRECATED grrsUpdatedTime "Use generic-lens or generic-optics with 'updatedTime' instead." #-}

-- | A description of the registry.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsDescription :: Lens.Lens' GetRegistryResponse (Lude.Maybe Lude.Text)
grrsDescription = Lens.lens (description :: GetRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetRegistryResponse)
{-# DEPRECATED grrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetRegistryResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetRegistryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRegistryResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
