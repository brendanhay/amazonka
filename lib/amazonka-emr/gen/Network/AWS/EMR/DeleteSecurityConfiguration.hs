{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DeleteSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security configuration.
module Network.AWS.EMR.DeleteSecurityConfiguration
  ( -- * Creating a request
    DeleteSecurityConfiguration (..),
    mkDeleteSecurityConfiguration,

    -- ** Request lenses
    dName,

    -- * Destructuring the response
    DeleteSecurityConfigurationResponse (..),
    mkDeleteSecurityConfigurationResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSecurityConfiguration' smart constructor.
newtype DeleteSecurityConfiguration = DeleteSecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSecurityConfiguration' with the minimum fields required to make a request.
--
-- * 'name' - The name of the security configuration.
mkDeleteSecurityConfiguration ::
  -- | 'name'
  Lude.Text ->
  DeleteSecurityConfiguration
mkDeleteSecurityConfiguration pName_ =
  DeleteSecurityConfiguration' {name = pName_}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DeleteSecurityConfiguration Lude.Text
dName = Lens.lens (name :: DeleteSecurityConfiguration -> Lude.Text) (\s a -> s {name = a} :: DeleteSecurityConfiguration)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteSecurityConfiguration where
  type
    Rs DeleteSecurityConfiguration =
      DeleteSecurityConfigurationResponse
  request = Req.postJSON emrService
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
              Lude.=# ( "ElasticMapReduce.DeleteSecurityConfiguration" ::
                          Lude.ByteString
                      ),
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
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
drsResponseStatus :: Lens.Lens' DeleteSecurityConfigurationResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteSecurityConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSecurityConfigurationResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
