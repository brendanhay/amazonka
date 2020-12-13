{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a build. This operation permanently deletes the build resource and any uploaded build files. Deleting a build does not affect the status of any active fleets using the build, but you can no longer create new fleets with the deleted build.
--
-- To delete a build, specify the build ID.
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
module Network.AWS.GameLift.DeleteBuild
  ( -- * Creating a request
    DeleteBuild (..),
    mkDeleteBuild,

    -- ** Request lenses
    dBuildId,

    -- * Destructuring the response
    DeleteBuildResponse (..),
    mkDeleteBuildResponse,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteBuild' smart constructor.
newtype DeleteBuild = DeleteBuild'
  { -- | A unique identifier for a build to delete. You can use either the build ID or ARN value.
    buildId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBuild' with the minimum fields required to make a request.
--
-- * 'buildId' - A unique identifier for a build to delete. You can use either the build ID or ARN value.
mkDeleteBuild ::
  -- | 'buildId'
  Lude.Text ->
  DeleteBuild
mkDeleteBuild pBuildId_ = DeleteBuild' {buildId = pBuildId_}

-- | A unique identifier for a build to delete. You can use either the build ID or ARN value.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBuildId :: Lens.Lens' DeleteBuild Lude.Text
dBuildId = Lens.lens (buildId :: DeleteBuild -> Lude.Text) (\s a -> s {buildId = a} :: DeleteBuild)
{-# DEPRECATED dBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

instance Lude.AWSRequest DeleteBuild where
  type Rs DeleteBuild = DeleteBuildResponse
  request = Req.postJSON gameLiftService
  response = Res.receiveNull DeleteBuildResponse'

instance Lude.ToHeaders DeleteBuild where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteBuild" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBuild where
  toJSON DeleteBuild' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BuildId" Lude..= buildId)])

instance Lude.ToPath DeleteBuild where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBuild where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBuildResponse' smart constructor.
data DeleteBuildResponse = DeleteBuildResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBuildResponse' with the minimum fields required to make a request.
mkDeleteBuildResponse ::
  DeleteBuildResponse
mkDeleteBuildResponse = DeleteBuildResponse'
