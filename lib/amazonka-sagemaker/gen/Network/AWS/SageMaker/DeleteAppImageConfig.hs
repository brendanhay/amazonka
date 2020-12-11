{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AppImageConfig.
module Network.AWS.SageMaker.DeleteAppImageConfig
  ( -- * Creating a request
    DeleteAppImageConfig (..),
    mkDeleteAppImageConfig,

    -- ** Request lenses
    daicAppImageConfigName,

    -- * Destructuring the response
    DeleteAppImageConfigResponse (..),
    mkDeleteAppImageConfigResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteAppImageConfig' smart constructor.
newtype DeleteAppImageConfig = DeleteAppImageConfig'
  { appImageConfigName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppImageConfig' with the minimum fields required to make a request.
--
-- * 'appImageConfigName' - The name of the AppImageConfig to delete.
mkDeleteAppImageConfig ::
  -- | 'appImageConfigName'
  Lude.Text ->
  DeleteAppImageConfig
mkDeleteAppImageConfig pAppImageConfigName_ =
  DeleteAppImageConfig' {appImageConfigName = pAppImageConfigName_}

-- | The name of the AppImageConfig to delete.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicAppImageConfigName :: Lens.Lens' DeleteAppImageConfig Lude.Text
daicAppImageConfigName = Lens.lens (appImageConfigName :: DeleteAppImageConfig -> Lude.Text) (\s a -> s {appImageConfigName = a} :: DeleteAppImageConfig)
{-# DEPRECATED daicAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

instance Lude.AWSRequest DeleteAppImageConfig where
  type Rs DeleteAppImageConfig = DeleteAppImageConfigResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteAppImageConfigResponse'

instance Lude.ToHeaders DeleteAppImageConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteAppImageConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAppImageConfig where
  toJSON DeleteAppImageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AppImageConfigName" Lude..= appImageConfigName)]
      )

instance Lude.ToPath DeleteAppImageConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAppImageConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppImageConfigResponse' smart constructor.
data DeleteAppImageConfigResponse = DeleteAppImageConfigResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppImageConfigResponse' with the minimum fields required to make a request.
mkDeleteAppImageConfigResponse ::
  DeleteAppImageConfigResponse
mkDeleteAppImageConfigResponse = DeleteAppImageConfigResponse'
