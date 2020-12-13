{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.CreateSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security configuration, which is stored in the service and can be specified when a cluster is created.
module Network.AWS.EMR.CreateSecurityConfiguration
  ( -- * Creating a request
    CreateSecurityConfiguration (..),
    mkCreateSecurityConfiguration,

    -- ** Request lenses
    cscSecurityConfiguration,
    cscName,

    -- * Destructuring the response
    CreateSecurityConfigurationResponse (..),
    mkCreateSecurityConfigurationResponse,

    -- ** Response lenses
    cscrsName,
    cscrsCreationDateTime,
    cscrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { -- | The security configuration details in JSON format. For JSON parameters and examples, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
    securityConfiguration :: Lude.Text,
    -- | The name of the security configuration.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityConfiguration' with the minimum fields required to make a request.
--
-- * 'securityConfiguration' - The security configuration details in JSON format. For JSON parameters and examples, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
-- * 'name' - The name of the security configuration.
mkCreateSecurityConfiguration ::
  -- | 'securityConfiguration'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateSecurityConfiguration
mkCreateSecurityConfiguration pSecurityConfiguration_ pName_ =
  CreateSecurityConfiguration'
    { securityConfiguration =
        pSecurityConfiguration_,
      name = pName_
    }

-- | The security configuration details in JSON format. For JSON parameters and examples, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscSecurityConfiguration :: Lens.Lens' CreateSecurityConfiguration Lude.Text
cscSecurityConfiguration = Lens.lens (securityConfiguration :: CreateSecurityConfiguration -> Lude.Text) (\s a -> s {securityConfiguration = a} :: CreateSecurityConfiguration)
{-# DEPRECATED cscSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscName :: Lens.Lens' CreateSecurityConfiguration Lude.Text
cscName = Lens.lens (name :: CreateSecurityConfiguration -> Lude.Text) (\s a -> s {name = a} :: CreateSecurityConfiguration)
{-# DEPRECATED cscName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateSecurityConfiguration where
  type
    Rs CreateSecurityConfiguration =
      CreateSecurityConfigurationResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSecurityConfigurationResponse'
            Lude.<$> (x Lude..:> "Name")
            Lude.<*> (x Lude..:> "CreationDateTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSecurityConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ElasticMapReduce.CreateSecurityConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSecurityConfiguration where
  toJSON CreateSecurityConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SecurityConfiguration" Lude..= securityConfiguration),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateSecurityConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSecurityConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { -- | The name of the security configuration.
    name :: Lude.Text,
    -- | The date and time the security configuration was created.
    creationDateTime :: Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the security configuration.
-- * 'creationDateTime' - The date and time the security configuration was created.
-- * 'responseStatus' - The response status code.
mkCreateSecurityConfigurationResponse ::
  -- | 'name'
  Lude.Text ->
  -- | 'creationDateTime'
  Lude.Timestamp ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateSecurityConfigurationResponse
mkCreateSecurityConfigurationResponse
  pName_
  pCreationDateTime_
  pResponseStatus_ =
    CreateSecurityConfigurationResponse'
      { name = pName_,
        creationDateTime = pCreationDateTime_,
        responseStatus = pResponseStatus_
      }

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrsName :: Lens.Lens' CreateSecurityConfigurationResponse Lude.Text
cscrsName = Lens.lens (name :: CreateSecurityConfigurationResponse -> Lude.Text) (\s a -> s {name = a} :: CreateSecurityConfigurationResponse)
{-# DEPRECATED cscrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time the security configuration was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrsCreationDateTime :: Lens.Lens' CreateSecurityConfigurationResponse Lude.Timestamp
cscrsCreationDateTime = Lens.lens (creationDateTime :: CreateSecurityConfigurationResponse -> Lude.Timestamp) (\s a -> s {creationDateTime = a} :: CreateSecurityConfigurationResponse)
{-# DEPRECATED cscrsCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrsResponseStatus :: Lens.Lens' CreateSecurityConfigurationResponse Lude.Int
cscrsResponseStatus = Lens.lens (responseStatus :: CreateSecurityConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSecurityConfigurationResponse)
{-# DEPRECATED cscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
