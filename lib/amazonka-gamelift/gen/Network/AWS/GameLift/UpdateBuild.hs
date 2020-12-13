{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata in a build resource, including the build name and version. To update the metadata, specify the build ID to update and provide the new values. If successful, a build object containing the updated metadata is returned.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
-- __Related operations__
--
--     * 'CreateBuild'
--
--
--     * 'ListBuilds'
--
--
--     * 'DescribeBuild'
--
--
--     * 'UpdateBuild'
--
--
--     * 'DeleteBuild'
module Network.AWS.GameLift.UpdateBuild
  ( -- * Creating a request
    UpdateBuild (..),
    mkUpdateBuild,

    -- ** Request lenses
    ubBuildId,
    ubName,
    ubVersion,

    -- * Destructuring the response
    UpdateBuildResponse (..),
    mkUpdateBuildResponse,

    -- ** Response lenses
    ubrsBuild,
    ubrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateBuild' smart constructor.
data UpdateBuild = UpdateBuild'
  { -- | A unique identifier for a build to update. You can use either the build ID or ARN value.
    buildId :: Lude.Text,
    -- | A descriptive label that is associated with a build. Build names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | Version information that is associated with a build or script. Version strings do not need to be unique.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBuild' with the minimum fields required to make a request.
--
-- * 'buildId' - A unique identifier for a build to update. You can use either the build ID or ARN value.
-- * 'name' - A descriptive label that is associated with a build. Build names do not need to be unique.
-- * 'version' - Version information that is associated with a build or script. Version strings do not need to be unique.
mkUpdateBuild ::
  -- | 'buildId'
  Lude.Text ->
  UpdateBuild
mkUpdateBuild pBuildId_ =
  UpdateBuild'
    { buildId = pBuildId_,
      name = Lude.Nothing,
      version = Lude.Nothing
    }

-- | A unique identifier for a build to update. You can use either the build ID or ARN value.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubBuildId :: Lens.Lens' UpdateBuild Lude.Text
ubBuildId = Lens.lens (buildId :: UpdateBuild -> Lude.Text) (\s a -> s {buildId = a} :: UpdateBuild)
{-# DEPRECATED ubBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

-- | A descriptive label that is associated with a build. Build names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubName :: Lens.Lens' UpdateBuild (Lude.Maybe Lude.Text)
ubName = Lens.lens (name :: UpdateBuild -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateBuild)
{-# DEPRECATED ubName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Version information that is associated with a build or script. Version strings do not need to be unique.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubVersion :: Lens.Lens' UpdateBuild (Lude.Maybe Lude.Text)
ubVersion = Lens.lens (version :: UpdateBuild -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: UpdateBuild)
{-# DEPRECATED ubVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest UpdateBuild where
  type Rs UpdateBuild = UpdateBuildResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateBuildResponse'
            Lude.<$> (x Lude..?> "Build") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBuild where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateBuild" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBuild where
  toJSON UpdateBuild' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BuildId" Lude..= buildId),
            ("Name" Lude..=) Lude.<$> name,
            ("Version" Lude..=) Lude.<$> version
          ]
      )

instance Lude.ToPath UpdateBuild where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateBuild where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateBuildResponse' smart constructor.
data UpdateBuildResponse = UpdateBuildResponse'
  { -- | The updated build resource.
    build :: Lude.Maybe Build,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBuildResponse' with the minimum fields required to make a request.
--
-- * 'build' - The updated build resource.
-- * 'responseStatus' - The response status code.
mkUpdateBuildResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBuildResponse
mkUpdateBuildResponse pResponseStatus_ =
  UpdateBuildResponse'
    { build = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated build resource.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsBuild :: Lens.Lens' UpdateBuildResponse (Lude.Maybe Build)
ubrsBuild = Lens.lens (build :: UpdateBuildResponse -> Lude.Maybe Build) (\s a -> s {build = a} :: UpdateBuildResponse)
{-# DEPRECATED ubrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsResponseStatus :: Lens.Lens' UpdateBuildResponse Lude.Int
ubrsResponseStatus = Lens.lens (responseStatus :: UpdateBuildResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBuildResponse)
{-# DEPRECATED ubrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
