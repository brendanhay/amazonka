{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified security configuration.
module Network.AWS.Glue.DeleteSecurityConfiguration
  ( -- * Creating a request
    DeleteSecurityConfiguration (..),
    mkDeleteSecurityConfiguration,

    -- ** Request lenses
    dscName,

    -- * Destructuring the response
    DeleteSecurityConfigurationResponse (..),
    mkDeleteSecurityConfigurationResponse,

    -- ** Response lenses
    dscrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSecurityConfiguration' smart constructor.
newtype DeleteSecurityConfiguration = DeleteSecurityConfiguration'
  { name ::
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

-- | Creates a value of 'DeleteSecurityConfiguration' with the minimum fields required to make a request.
--
-- * 'name' - The name of the security configuration to delete.
mkDeleteSecurityConfiguration ::
  -- | 'name'
  Lude.Text ->
  DeleteSecurityConfiguration
mkDeleteSecurityConfiguration pName_ =
  DeleteSecurityConfiguration' {name = pName_}

-- | The name of the security configuration to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscName :: Lens.Lens' DeleteSecurityConfiguration Lude.Text
dscName = Lens.lens (name :: DeleteSecurityConfiguration -> Lude.Text) (\s a -> s {name = a} :: DeleteSecurityConfiguration)
{-# DEPRECATED dscName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteSecurityConfiguration where
  type
    Rs DeleteSecurityConfiguration =
      DeleteSecurityConfigurationResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteSecurityConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSecurityConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteSecurityConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSecurityConfiguration where
  toJSON DeleteSecurityConfiguration' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteSecurityConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSecurityConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSecurityConfigurationResponse' smart constructor.
newtype DeleteSecurityConfigurationResponse = DeleteSecurityConfigurationResponse'
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

-- | Creates a value of 'DeleteSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteSecurityConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSecurityConfigurationResponse
mkDeleteSecurityConfigurationResponse pResponseStatus_ =
  DeleteSecurityConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsResponseStatus :: Lens.Lens' DeleteSecurityConfigurationResponse Lude.Int
dscrsResponseStatus = Lens.lens (responseStatus :: DeleteSecurityConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSecurityConfigurationResponse)
{-# DEPRECATED dscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
