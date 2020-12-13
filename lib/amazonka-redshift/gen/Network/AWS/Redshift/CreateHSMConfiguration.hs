{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateHSMConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM configuration that contains the information required by an Amazon Redshift cluster to store and use database encryption keys in a Hardware Security Module (HSM). After creating the HSM configuration, you can specify it as a parameter when creating a cluster. The cluster will then store its encryption keys in the HSM.
--
-- In addition to creating an HSM configuration, you must also create an HSM client certificate. For more information, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules> in the Amazon Redshift Cluster Management Guide.
module Network.AWS.Redshift.CreateHSMConfiguration
  ( -- * Creating a request
    CreateHSMConfiguration (..),
    mkCreateHSMConfiguration,

    -- ** Request lenses
    chcHSMServerPublicCertificate,
    chcHSMConfigurationIdentifier,
    chcHSMPartitionName,
    chcHSMPartitionPassword,
    chcDescription,
    chcTags,
    chcHSMIPAddress,

    -- * Destructuring the response
    CreateHSMConfigurationResponse (..),
    mkCreateHSMConfigurationResponse,

    -- ** Response lenses
    chcrsHSMConfiguration,
    chcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateHSMConfiguration' smart constructor.
data CreateHSMConfiguration = CreateHSMConfiguration'
  { -- | The HSMs public certificate file. When using Cloud HSM, the file name is server.pem.
    hsmServerPublicCertificate :: Lude.Text,
    -- | The identifier to be assigned to the new Amazon Redshift HSM configuration.
    hsmConfigurationIdentifier :: Lude.Text,
    -- | The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
    hsmPartitionName :: Lude.Text,
    -- | The password required to access the HSM partition.
    hsmPartitionPassword :: Lude.Text,
    -- | A text description of the HSM configuration to be created.
    description :: Lude.Text,
    -- | A list of tag instances.
    tags :: Lude.Maybe [Tag],
    -- | The IP address that the Amazon Redshift cluster must use to access the HSM.
    hsmIPAddress :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHSMConfiguration' with the minimum fields required to make a request.
--
-- * 'hsmServerPublicCertificate' - The HSMs public certificate file. When using Cloud HSM, the file name is server.pem.
-- * 'hsmConfigurationIdentifier' - The identifier to be assigned to the new Amazon Redshift HSM configuration.
-- * 'hsmPartitionName' - The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
-- * 'hsmPartitionPassword' - The password required to access the HSM partition.
-- * 'description' - A text description of the HSM configuration to be created.
-- * 'tags' - A list of tag instances.
-- * 'hsmIPAddress' - The IP address that the Amazon Redshift cluster must use to access the HSM.
mkCreateHSMConfiguration ::
  -- | 'hsmServerPublicCertificate'
  Lude.Text ->
  -- | 'hsmConfigurationIdentifier'
  Lude.Text ->
  -- | 'hsmPartitionName'
  Lude.Text ->
  -- | 'hsmPartitionPassword'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  -- | 'hsmIPAddress'
  Lude.Text ->
  CreateHSMConfiguration
mkCreateHSMConfiguration
  pHSMServerPublicCertificate_
  pHSMConfigurationIdentifier_
  pHSMPartitionName_
  pHSMPartitionPassword_
  pDescription_
  pHSMIPAddress_ =
    CreateHSMConfiguration'
      { hsmServerPublicCertificate =
          pHSMServerPublicCertificate_,
        hsmConfigurationIdentifier = pHSMConfigurationIdentifier_,
        hsmPartitionName = pHSMPartitionName_,
        hsmPartitionPassword = pHSMPartitionPassword_,
        description = pDescription_,
        tags = Lude.Nothing,
        hsmIPAddress = pHSMIPAddress_
      }

-- | The HSMs public certificate file. When using Cloud HSM, the file name is server.pem.
--
-- /Note:/ Consider using 'hsmServerPublicCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHSMServerPublicCertificate :: Lens.Lens' CreateHSMConfiguration Lude.Text
chcHSMServerPublicCertificate = Lens.lens (hsmServerPublicCertificate :: CreateHSMConfiguration -> Lude.Text) (\s a -> s {hsmServerPublicCertificate = a} :: CreateHSMConfiguration)
{-# DEPRECATED chcHSMServerPublicCertificate "Use generic-lens or generic-optics with 'hsmServerPublicCertificate' instead." #-}

-- | The identifier to be assigned to the new Amazon Redshift HSM configuration.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHSMConfigurationIdentifier :: Lens.Lens' CreateHSMConfiguration Lude.Text
chcHSMConfigurationIdentifier = Lens.lens (hsmConfigurationIdentifier :: CreateHSMConfiguration -> Lude.Text) (\s a -> s {hsmConfigurationIdentifier = a} :: CreateHSMConfiguration)
{-# DEPRECATED chcHSMConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

-- | The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
--
-- /Note:/ Consider using 'hsmPartitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHSMPartitionName :: Lens.Lens' CreateHSMConfiguration Lude.Text
chcHSMPartitionName = Lens.lens (hsmPartitionName :: CreateHSMConfiguration -> Lude.Text) (\s a -> s {hsmPartitionName = a} :: CreateHSMConfiguration)
{-# DEPRECATED chcHSMPartitionName "Use generic-lens or generic-optics with 'hsmPartitionName' instead." #-}

-- | The password required to access the HSM partition.
--
-- /Note:/ Consider using 'hsmPartitionPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHSMPartitionPassword :: Lens.Lens' CreateHSMConfiguration Lude.Text
chcHSMPartitionPassword = Lens.lens (hsmPartitionPassword :: CreateHSMConfiguration -> Lude.Text) (\s a -> s {hsmPartitionPassword = a} :: CreateHSMConfiguration)
{-# DEPRECATED chcHSMPartitionPassword "Use generic-lens or generic-optics with 'hsmPartitionPassword' instead." #-}

-- | A text description of the HSM configuration to be created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcDescription :: Lens.Lens' CreateHSMConfiguration Lude.Text
chcDescription = Lens.lens (description :: CreateHSMConfiguration -> Lude.Text) (\s a -> s {description = a} :: CreateHSMConfiguration)
{-# DEPRECATED chcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcTags :: Lens.Lens' CreateHSMConfiguration (Lude.Maybe [Tag])
chcTags = Lens.lens (tags :: CreateHSMConfiguration -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateHSMConfiguration)
{-# DEPRECATED chcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
--
-- /Note:/ Consider using 'hsmIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHSMIPAddress :: Lens.Lens' CreateHSMConfiguration Lude.Text
chcHSMIPAddress = Lens.lens (hsmIPAddress :: CreateHSMConfiguration -> Lude.Text) (\s a -> s {hsmIPAddress = a} :: CreateHSMConfiguration)
{-# DEPRECATED chcHSMIPAddress "Use generic-lens or generic-optics with 'hsmIPAddress' instead." #-}

instance Lude.AWSRequest CreateHSMConfiguration where
  type Rs CreateHSMConfiguration = CreateHSMConfigurationResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateHsmConfigurationResult"
      ( \s h x ->
          CreateHSMConfigurationResponse'
            Lude.<$> (x Lude..@? "HsmConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateHSMConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateHSMConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateHSMConfiguration where
  toQuery CreateHSMConfiguration' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateHsmConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "HsmServerPublicCertificate" Lude.=: hsmServerPublicCertificate,
        "HsmConfigurationIdentifier" Lude.=: hsmConfigurationIdentifier,
        "HsmPartitionName" Lude.=: hsmPartitionName,
        "HsmPartitionPassword" Lude.=: hsmPartitionPassword,
        "Description" Lude.=: description,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "HsmIpAddress" Lude.=: hsmIPAddress
      ]

-- | /See:/ 'mkCreateHSMConfigurationResponse' smart constructor.
data CreateHSMConfigurationResponse = CreateHSMConfigurationResponse'
  { hsmConfiguration :: Lude.Maybe HSMConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHSMConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'hsmConfiguration' -
-- * 'responseStatus' - The response status code.
mkCreateHSMConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateHSMConfigurationResponse
mkCreateHSMConfigurationResponse pResponseStatus_ =
  CreateHSMConfigurationResponse'
    { hsmConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'hsmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrsHSMConfiguration :: Lens.Lens' CreateHSMConfigurationResponse (Lude.Maybe HSMConfiguration)
chcrsHSMConfiguration = Lens.lens (hsmConfiguration :: CreateHSMConfigurationResponse -> Lude.Maybe HSMConfiguration) (\s a -> s {hsmConfiguration = a} :: CreateHSMConfigurationResponse)
{-# DEPRECATED chcrsHSMConfiguration "Use generic-lens or generic-optics with 'hsmConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrsResponseStatus :: Lens.Lens' CreateHSMConfigurationResponse Lude.Int
chcrsResponseStatus = Lens.lens (responseStatus :: CreateHSMConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHSMConfigurationResponse)
{-# DEPRECATED chcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
