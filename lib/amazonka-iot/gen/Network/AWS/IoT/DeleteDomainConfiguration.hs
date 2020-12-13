{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain configuration.
module Network.AWS.IoT.DeleteDomainConfiguration
  ( -- * Creating a request
    DeleteDomainConfiguration (..),
    mkDeleteDomainConfiguration,

    -- ** Request lenses
    dDomainConfigurationName,

    -- * Destructuring the response
    DeleteDomainConfigurationResponse (..),
    mkDeleteDomainConfigurationResponse,

    -- ** Response lenses
    ddcfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDomainConfiguration' smart constructor.
newtype DeleteDomainConfiguration = DeleteDomainConfiguration'
  { -- | The name of the domain configuration to be deleted.
    domainConfigurationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomainConfiguration' with the minimum fields required to make a request.
--
-- * 'domainConfigurationName' - The name of the domain configuration to be deleted.
mkDeleteDomainConfiguration ::
  -- | 'domainConfigurationName'
  Lude.Text ->
  DeleteDomainConfiguration
mkDeleteDomainConfiguration pDomainConfigurationName_ =
  DeleteDomainConfiguration'
    { domainConfigurationName =
        pDomainConfigurationName_
    }

-- | The name of the domain configuration to be deleted.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainConfigurationName :: Lens.Lens' DeleteDomainConfiguration Lude.Text
dDomainConfigurationName = Lens.lens (domainConfigurationName :: DeleteDomainConfiguration -> Lude.Text) (\s a -> s {domainConfigurationName = a} :: DeleteDomainConfiguration)
{-# DEPRECATED dDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

instance Lude.AWSRequest DeleteDomainConfiguration where
  type
    Rs DeleteDomainConfiguration =
      DeleteDomainConfigurationResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDomainConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDomainConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDomainConfiguration where
  toPath DeleteDomainConfiguration' {..} =
    Lude.mconcat
      ["/domainConfigurations/", Lude.toBS domainConfigurationName]

instance Lude.ToQuery DeleteDomainConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDomainConfigurationResponse' smart constructor.
newtype DeleteDomainConfigurationResponse = DeleteDomainConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomainConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDomainConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDomainConfigurationResponse
mkDeleteDomainConfigurationResponse pResponseStatus_ =
  DeleteDomainConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcfrsResponseStatus :: Lens.Lens' DeleteDomainConfigurationResponse Lude.Int
ddcfrsResponseStatus = Lens.lens (responseStatus :: DeleteDomainConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDomainConfigurationResponse)
{-# DEPRECATED ddcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
