{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteLoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the 'LoggingConfiguration' from the specified web ACL.
module Network.AWS.WAF.DeleteLoggingConfiguration
  ( -- * Creating a request
    DeleteLoggingConfiguration (..),
    mkDeleteLoggingConfiguration,

    -- ** Request lenses
    dlcResourceARN,

    -- * Destructuring the response
    DeleteLoggingConfigurationResponse (..),
    mkDeleteLoggingConfigurationResponse,

    -- ** Response lenses
    dlcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkDeleteLoggingConfiguration' smart constructor.
newtype DeleteLoggingConfiguration = DeleteLoggingConfiguration'
  { resourceARN ::
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

-- | Creates a value of 'DeleteLoggingConfiguration' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the web ACL from which you want to delete the 'LoggingConfiguration' .
mkDeleteLoggingConfiguration ::
  -- | 'resourceARN'
  Lude.Text ->
  DeleteLoggingConfiguration
mkDeleteLoggingConfiguration pResourceARN_ =
  DeleteLoggingConfiguration' {resourceARN = pResourceARN_}

-- | The Amazon Resource Name (ARN) of the web ACL from which you want to delete the 'LoggingConfiguration' .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcResourceARN :: Lens.Lens' DeleteLoggingConfiguration Lude.Text
dlcResourceARN = Lens.lens (resourceARN :: DeleteLoggingConfiguration -> Lude.Text) (\s a -> s {resourceARN = a} :: DeleteLoggingConfiguration)
{-# DEPRECATED dlcResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest DeleteLoggingConfiguration where
  type
    Rs DeleteLoggingConfiguration =
      DeleteLoggingConfigurationResponse
  request = Req.postJSON wafService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteLoggingConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLoggingConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.DeleteLoggingConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLoggingConfiguration where
  toJSON DeleteLoggingConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceArn" Lude..= resourceARN)])

instance Lude.ToPath DeleteLoggingConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLoggingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLoggingConfigurationResponse' smart constructor.
newtype DeleteLoggingConfigurationResponse = DeleteLoggingConfigurationResponse'
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

-- | Creates a value of 'DeleteLoggingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLoggingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLoggingConfigurationResponse
mkDeleteLoggingConfigurationResponse pResponseStatus_ =
  DeleteLoggingConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrsResponseStatus :: Lens.Lens' DeleteLoggingConfigurationResponse Lude.Int
dlcrsResponseStatus = Lens.lens (responseStatus :: DeleteLoggingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLoggingConfigurationResponse)
{-# DEPRECATED dlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
