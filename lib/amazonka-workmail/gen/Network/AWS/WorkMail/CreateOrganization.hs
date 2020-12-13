{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon WorkMail organization. Optionally, you can choose to associate an existing AWS Directory Service directory with your organization. If an AWS Directory Service directory ID is specified, the organization alias must match the directory alias. If you choose not to associate an existing directory with your organization, then we create a new Amazon WorkMail directory for you. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_new_organization.html Adding an organization> in the /Amazon WorkMail Administrator Guide/ .
--
-- You can associate multiple email domains with an organization, then set your default email domain from the Amazon WorkMail console. You can also associate a domain that is managed in an Amazon Route 53 public hosted zone. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain> and <https://docs.aws.amazon.com/workmail/latest/adminguide/default_domain.html Choosing the default domain> in the /Amazon WorkMail Administrator Guide/ .
-- Optionally, you can use a customer managed master key from AWS Key Management Service (AWS KMS) to encrypt email for your organization. If you don't associate an AWS KMS key, Amazon WorkMail creates a default AWS managed master key for you.
module Network.AWS.WorkMail.CreateOrganization
  ( -- * Creating a request
    CreateOrganization (..),
    mkCreateOrganization,

    -- ** Request lenses
    coDirectoryId,
    coEnableInteroperability,
    coKMSKeyARN,
    coClientToken,
    coAlias,
    coDomains,

    -- * Destructuring the response
    CreateOrganizationResponse (..),
    mkCreateOrganizationResponse,

    -- ** Response lenses
    corsOrganizationId,
    corsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkCreateOrganization' smart constructor.
data CreateOrganization = CreateOrganization'
  { -- | The AWS Directory Service directory ID.
    directoryId :: Lude.Maybe Lude.Text,
    -- | When @true@ , allows organization interoperability between Amazon WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD Connector directory ID is included in the request.
    enableInteroperability :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of a customer managed master key from AWS KMS.
    kmsKeyARN :: Lude.Maybe Lude.Text,
    -- | The idempotency token associated with the request.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The organization alias.
    alias :: Lude.Text,
    -- | The email domains to associate with the organization.
    domains :: Lude.Maybe [Domain]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOrganization' with the minimum fields required to make a request.
--
-- * 'directoryId' - The AWS Directory Service directory ID.
-- * 'enableInteroperability' - When @true@ , allows organization interoperability between Amazon WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD Connector directory ID is included in the request.
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of a customer managed master key from AWS KMS.
-- * 'clientToken' - The idempotency token associated with the request.
-- * 'alias' - The organization alias.
-- * 'domains' - The email domains to associate with the organization.
mkCreateOrganization ::
  -- | 'alias'
  Lude.Text ->
  CreateOrganization
mkCreateOrganization pAlias_ =
  CreateOrganization'
    { directoryId = Lude.Nothing,
      enableInteroperability = Lude.Nothing,
      kmsKeyARN = Lude.Nothing,
      clientToken = Lude.Nothing,
      alias = pAlias_,
      domains = Lude.Nothing
    }

-- | The AWS Directory Service directory ID.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coDirectoryId :: Lens.Lens' CreateOrganization (Lude.Maybe Lude.Text)
coDirectoryId = Lens.lens (directoryId :: CreateOrganization -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: CreateOrganization)
{-# DEPRECATED coDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | When @true@ , allows organization interoperability between Amazon WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD Connector directory ID is included in the request.
--
-- /Note:/ Consider using 'enableInteroperability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnableInteroperability :: Lens.Lens' CreateOrganization (Lude.Maybe Lude.Bool)
coEnableInteroperability = Lens.lens (enableInteroperability :: CreateOrganization -> Lude.Maybe Lude.Bool) (\s a -> s {enableInteroperability = a} :: CreateOrganization)
{-# DEPRECATED coEnableInteroperability "Use generic-lens or generic-optics with 'enableInteroperability' instead." #-}

-- | The Amazon Resource Name (ARN) of a customer managed master key from AWS KMS.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coKMSKeyARN :: Lens.Lens' CreateOrganization (Lude.Maybe Lude.Text)
coKMSKeyARN = Lens.lens (kmsKeyARN :: CreateOrganization -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: CreateOrganization)
{-# DEPRECATED coKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The idempotency token associated with the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coClientToken :: Lens.Lens' CreateOrganization (Lude.Maybe Lude.Text)
coClientToken = Lens.lens (clientToken :: CreateOrganization -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateOrganization)
{-# DEPRECATED coClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The organization alias.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coAlias :: Lens.Lens' CreateOrganization Lude.Text
coAlias = Lens.lens (alias :: CreateOrganization -> Lude.Text) (\s a -> s {alias = a} :: CreateOrganization)
{-# DEPRECATED coAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The email domains to associate with the organization.
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coDomains :: Lens.Lens' CreateOrganization (Lude.Maybe [Domain])
coDomains = Lens.lens (domains :: CreateOrganization -> Lude.Maybe [Domain]) (\s a -> s {domains = a} :: CreateOrganization)
{-# DEPRECATED coDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

instance Lude.AWSRequest CreateOrganization where
  type Rs CreateOrganization = CreateOrganizationResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateOrganizationResponse'
            Lude.<$> (x Lude..?> "OrganizationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.CreateOrganization" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateOrganization where
  toJSON CreateOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DirectoryId" Lude..=) Lude.<$> directoryId,
            ("EnableInteroperability" Lude..=) Lude.<$> enableInteroperability,
            ("KmsKeyArn" Lude..=) Lude.<$> kmsKeyARN,
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            Lude.Just ("Alias" Lude..= alias),
            ("Domains" Lude..=) Lude.<$> domains
          ]
      )

instance Lude.ToPath CreateOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { -- | The organization ID.
    organizationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'organizationId' - The organization ID.
-- * 'responseStatus' - The response status code.
mkCreateOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOrganizationResponse
mkCreateOrganizationResponse pResponseStatus_ =
  CreateOrganizationResponse'
    { organizationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsOrganizationId :: Lens.Lens' CreateOrganizationResponse (Lude.Maybe Lude.Text)
corsOrganizationId = Lens.lens (organizationId :: CreateOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: CreateOrganizationResponse)
{-# DEPRECATED corsOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsResponseStatus :: Lens.Lens' CreateOrganizationResponse Lude.Int
corsResponseStatus = Lens.lens (responseStatus :: CreateOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOrganizationResponse)
{-# DEPRECATED corsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
