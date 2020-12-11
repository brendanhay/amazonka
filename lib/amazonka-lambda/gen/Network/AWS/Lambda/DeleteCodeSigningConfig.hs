{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the code signing configuration. You can delete the code signing configuration only if no function is using it.
module Network.AWS.Lambda.DeleteCodeSigningConfig
  ( -- * Creating a request
    DeleteCodeSigningConfig (..),
    mkDeleteCodeSigningConfig,

    -- ** Request lenses
    dcscCodeSigningConfigARN,

    -- * Destructuring the response
    DeleteCodeSigningConfigResponse (..),
    mkDeleteCodeSigningConfigResponse,

    -- ** Response lenses
    dcscrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCodeSigningConfig' smart constructor.
newtype DeleteCodeSigningConfig = DeleteCodeSigningConfig'
  { codeSigningConfigARN ::
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

-- | Creates a value of 'DeleteCodeSigningConfig' with the minimum fields required to make a request.
--
-- * 'codeSigningConfigARN' - The The Amazon Resource Name (ARN) of the code signing configuration.
mkDeleteCodeSigningConfig ::
  -- | 'codeSigningConfigARN'
  Lude.Text ->
  DeleteCodeSigningConfig
mkDeleteCodeSigningConfig pCodeSigningConfigARN_ =
  DeleteCodeSigningConfig'
    { codeSigningConfigARN =
        pCodeSigningConfigARN_
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscCodeSigningConfigARN :: Lens.Lens' DeleteCodeSigningConfig Lude.Text
dcscCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: DeleteCodeSigningConfig -> Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: DeleteCodeSigningConfig)
{-# DEPRECATED dcscCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

instance Lude.AWSRequest DeleteCodeSigningConfig where
  type Rs DeleteCodeSigningConfig = DeleteCodeSigningConfigResponse
  request = Req.delete lambdaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteCodeSigningConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCodeSigningConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCodeSigningConfig where
  toPath DeleteCodeSigningConfig' {..} =
    Lude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Lude.toBS codeSigningConfigARN
      ]

instance Lude.ToQuery DeleteCodeSigningConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCodeSigningConfigResponse' smart constructor.
newtype DeleteCodeSigningConfigResponse = DeleteCodeSigningConfigResponse'
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

-- | Creates a value of 'DeleteCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteCodeSigningConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCodeSigningConfigResponse
mkDeleteCodeSigningConfigResponse pResponseStatus_ =
  DeleteCodeSigningConfigResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrsResponseStatus :: Lens.Lens' DeleteCodeSigningConfigResponse Lude.Int
dcscrsResponseStatus = Lens.lens (responseStatus :: DeleteCodeSigningConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCodeSigningConfigResponse)
{-# DEPRECATED dcscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
