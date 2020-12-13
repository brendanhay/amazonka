{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for a custom game build. To request a build resource, specify a build ID. If successful, an object containing the build properties is returned.
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
module Network.AWS.GameLift.DescribeBuild
  ( -- * Creating a request
    DescribeBuild (..),
    mkDescribeBuild,

    -- ** Request lenses
    dbBuildId,

    -- * Destructuring the response
    DescribeBuildResponse (..),
    mkDescribeBuildResponse,

    -- ** Response lenses
    dbrsBuild,
    dbrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeBuild' smart constructor.
newtype DescribeBuild = DescribeBuild'
  { -- | A unique identifier for a build to retrieve properties for. You can use either the build ID or ARN value.
    buildId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBuild' with the minimum fields required to make a request.
--
-- * 'buildId' - A unique identifier for a build to retrieve properties for. You can use either the build ID or ARN value.
mkDescribeBuild ::
  -- | 'buildId'
  Lude.Text ->
  DescribeBuild
mkDescribeBuild pBuildId_ = DescribeBuild' {buildId = pBuildId_}

-- | A unique identifier for a build to retrieve properties for. You can use either the build ID or ARN value.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBuildId :: Lens.Lens' DescribeBuild Lude.Text
dbBuildId = Lens.lens (buildId :: DescribeBuild -> Lude.Text) (\s a -> s {buildId = a} :: DescribeBuild)
{-# DEPRECATED dbBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

instance Lude.AWSRequest DescribeBuild where
  type Rs DescribeBuild = DescribeBuildResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBuildResponse'
            Lude.<$> (x Lude..?> "Build") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBuild where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeBuild" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBuild where
  toJSON DescribeBuild' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BuildId" Lude..= buildId)])

instance Lude.ToPath DescribeBuild where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBuild where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeBuildResponse' smart constructor.
data DescribeBuildResponse = DescribeBuildResponse'
  { -- | Set of properties describing the requested build.
    build :: Lude.Maybe Build,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBuildResponse' with the minimum fields required to make a request.
--
-- * 'build' - Set of properties describing the requested build.
-- * 'responseStatus' - The response status code.
mkDescribeBuildResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBuildResponse
mkDescribeBuildResponse pResponseStatus_ =
  DescribeBuildResponse'
    { build = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Set of properties describing the requested build.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBuild :: Lens.Lens' DescribeBuildResponse (Lude.Maybe Build)
dbrsBuild = Lens.lens (build :: DescribeBuildResponse -> Lude.Maybe Build) (\s a -> s {build = a} :: DescribeBuildResponse)
{-# DEPRECATED dbrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DescribeBuildResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DescribeBuildResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBuildResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
