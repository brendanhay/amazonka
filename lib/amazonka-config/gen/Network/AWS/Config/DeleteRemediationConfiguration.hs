{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteRemediationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the remediation configuration.
module Network.AWS.Config.DeleteRemediationConfiguration
  ( -- * Creating a request
    DeleteRemediationConfiguration (..),
    mkDeleteRemediationConfiguration,

    -- ** Request lenses
    delResourceType,
    delConfigRuleName,

    -- * Destructuring the response
    DeleteRemediationConfigurationResponse (..),
    mkDeleteRemediationConfigurationResponse,

    -- ** Response lenses
    drcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRemediationConfiguration' smart constructor.
data DeleteRemediationConfiguration = DeleteRemediationConfiguration'
  { resourceType ::
      Lude.Maybe Lude.Text,
    configRuleName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRemediationConfiguration' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The name of the AWS Config rule for which you want to delete remediation configuration.
-- * 'resourceType' - The type of a resource.
mkDeleteRemediationConfiguration ::
  -- | 'configRuleName'
  Lude.Text ->
  DeleteRemediationConfiguration
mkDeleteRemediationConfiguration pConfigRuleName_ =
  DeleteRemediationConfiguration'
    { resourceType = Lude.Nothing,
      configRuleName = pConfigRuleName_
    }

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delResourceType :: Lens.Lens' DeleteRemediationConfiguration (Lude.Maybe Lude.Text)
delResourceType = Lens.lens (resourceType :: DeleteRemediationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: DeleteRemediationConfiguration)
{-# DEPRECATED delResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The name of the AWS Config rule for which you want to delete remediation configuration.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delConfigRuleName :: Lens.Lens' DeleteRemediationConfiguration Lude.Text
delConfigRuleName = Lens.lens (configRuleName :: DeleteRemediationConfiguration -> Lude.Text) (\s a -> s {configRuleName = a} :: DeleteRemediationConfiguration)
{-# DEPRECATED delConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Lude.AWSRequest DeleteRemediationConfiguration where
  type
    Rs DeleteRemediationConfiguration =
      DeleteRemediationConfigurationResponse
  request = Req.postJSON configService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteRemediationConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRemediationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeleteRemediationConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRemediationConfiguration where
  toJSON DeleteRemediationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            Lude.Just ("ConfigRuleName" Lude..= configRuleName)
          ]
      )

instance Lude.ToPath DeleteRemediationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRemediationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRemediationConfigurationResponse' smart constructor.
newtype DeleteRemediationConfigurationResponse = DeleteRemediationConfigurationResponse'
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

-- | Creates a value of 'DeleteRemediationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRemediationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRemediationConfigurationResponse
mkDeleteRemediationConfigurationResponse pResponseStatus_ =
  DeleteRemediationConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsResponseStatus :: Lens.Lens' DeleteRemediationConfigurationResponse Lude.Int
drcrsResponseStatus = Lens.lens (responseStatus :: DeleteRemediationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRemediationConfigurationResponse)
{-# DEPRECATED drcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
