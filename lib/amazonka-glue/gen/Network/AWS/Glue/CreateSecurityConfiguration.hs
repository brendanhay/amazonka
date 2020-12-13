{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new security configuration. A security configuration is a set of security properties that can be used by AWS Glue. You can use a security configuration to encrypt data at rest. For information about using security configurations in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/encryption-security-configuration.html Encrypting Data Written by Crawlers, Jobs, and Development Endpoints> .
module Network.AWS.Glue.CreateSecurityConfiguration
  ( -- * Creating a request
    CreateSecurityConfiguration (..),
    mkCreateSecurityConfiguration,

    -- ** Request lenses
    cscName,
    cscEncryptionConfiguration,

    -- * Destructuring the response
    CreateSecurityConfigurationResponse (..),
    mkCreateSecurityConfigurationResponse,

    -- ** Response lenses
    cscrsName,
    cscrsCreatedTimestamp,
    cscrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { -- | The name for the new security configuration.
    name :: Lude.Text,
    -- | The encryption configuration for the new security configuration.
    encryptionConfiguration :: EncryptionConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityConfiguration' with the minimum fields required to make a request.
--
-- * 'name' - The name for the new security configuration.
-- * 'encryptionConfiguration' - The encryption configuration for the new security configuration.
mkCreateSecurityConfiguration ::
  -- | 'name'
  Lude.Text ->
  -- | 'encryptionConfiguration'
  EncryptionConfiguration ->
  CreateSecurityConfiguration
mkCreateSecurityConfiguration pName_ pEncryptionConfiguration_ =
  CreateSecurityConfiguration'
    { name = pName_,
      encryptionConfiguration = pEncryptionConfiguration_
    }

-- | The name for the new security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscName :: Lens.Lens' CreateSecurityConfiguration Lude.Text
cscName = Lens.lens (name :: CreateSecurityConfiguration -> Lude.Text) (\s a -> s {name = a} :: CreateSecurityConfiguration)
{-# DEPRECATED cscName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The encryption configuration for the new security configuration.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscEncryptionConfiguration :: Lens.Lens' CreateSecurityConfiguration EncryptionConfiguration
cscEncryptionConfiguration = Lens.lens (encryptionConfiguration :: CreateSecurityConfiguration -> EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: CreateSecurityConfiguration)
{-# DEPRECATED cscEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

instance Lude.AWSRequest CreateSecurityConfiguration where
  type
    Rs CreateSecurityConfiguration =
      CreateSecurityConfigurationResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSecurityConfigurationResponse'
            Lude.<$> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreatedTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSecurityConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateSecurityConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSecurityConfiguration where
  toJSON CreateSecurityConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just
              ("EncryptionConfiguration" Lude..= encryptionConfiguration)
          ]
      )

instance Lude.ToPath CreateSecurityConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSecurityConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { -- | The name assigned to the new security configuration.
    name :: Lude.Maybe Lude.Text,
    -- | The time at which the new security configuration was created.
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name assigned to the new security configuration.
-- * 'createdTimestamp' - The time at which the new security configuration was created.
-- * 'responseStatus' - The response status code.
mkCreateSecurityConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSecurityConfigurationResponse
mkCreateSecurityConfigurationResponse pResponseStatus_ =
  CreateSecurityConfigurationResponse'
    { name = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name assigned to the new security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrsName :: Lens.Lens' CreateSecurityConfigurationResponse (Lude.Maybe Lude.Text)
cscrsName = Lens.lens (name :: CreateSecurityConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateSecurityConfigurationResponse)
{-# DEPRECATED cscrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time at which the new security configuration was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrsCreatedTimestamp :: Lens.Lens' CreateSecurityConfigurationResponse (Lude.Maybe Lude.Timestamp)
cscrsCreatedTimestamp = Lens.lens (createdTimestamp :: CreateSecurityConfigurationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: CreateSecurityConfigurationResponse)
{-# DEPRECATED cscrsCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrsResponseStatus :: Lens.Lens' CreateSecurityConfigurationResponse Lude.Int
cscrsResponseStatus = Lens.lens (responseStatus :: CreateSecurityConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSecurityConfigurationResponse)
{-# DEPRECATED cscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
