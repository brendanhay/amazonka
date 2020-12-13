{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.GetBlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Amazon EMR block public access configuration for your AWS account in the current Region. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/configure-block-public-access.html Configure Block Public Access for Amazon EMR> in the /Amazon EMR Management Guide/ .
module Network.AWS.EMR.GetBlockPublicAccessConfiguration
  ( -- * Creating a request
    GetBlockPublicAccessConfiguration (..),
    mkGetBlockPublicAccessConfiguration,

    -- * Destructuring the response
    GetBlockPublicAccessConfigurationResponse (..),
    mkGetBlockPublicAccessConfigurationResponse,

    -- ** Response lenses
    gbpacrsBlockPublicAccessConfiguration,
    gbpacrsBlockPublicAccessConfigurationMetadata,
    gbpacrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBlockPublicAccessConfiguration' smart constructor.
data GetBlockPublicAccessConfiguration = GetBlockPublicAccessConfiguration'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBlockPublicAccessConfiguration' with the minimum fields required to make a request.
mkGetBlockPublicAccessConfiguration ::
  GetBlockPublicAccessConfiguration
mkGetBlockPublicAccessConfiguration =
  GetBlockPublicAccessConfiguration'

instance Lude.AWSRequest GetBlockPublicAccessConfiguration where
  type
    Rs GetBlockPublicAccessConfiguration =
      GetBlockPublicAccessConfigurationResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBlockPublicAccessConfigurationResponse'
            Lude.<$> (x Lude..:> "BlockPublicAccessConfiguration")
            Lude.<*> (x Lude..:> "BlockPublicAccessConfigurationMetadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBlockPublicAccessConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ElasticMapReduce.GetBlockPublicAccessConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetBlockPublicAccessConfiguration where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetBlockPublicAccessConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetBlockPublicAccessConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBlockPublicAccessConfigurationResponse' smart constructor.
data GetBlockPublicAccessConfigurationResponse = GetBlockPublicAccessConfigurationResponse'
  { -- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating the block public access configuration to remove the exception.
    blockPublicAccessConfiguration :: BlockPublicAccessConfiguration,
    -- | Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
    blockPublicAccessConfigurationMetadata :: BlockPublicAccessConfigurationMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBlockPublicAccessConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'blockPublicAccessConfiguration' - A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating the block public access configuration to remove the exception.
-- * 'blockPublicAccessConfigurationMetadata' - Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
-- * 'responseStatus' - The response status code.
mkGetBlockPublicAccessConfigurationResponse ::
  -- | 'blockPublicAccessConfiguration'
  BlockPublicAccessConfiguration ->
  -- | 'blockPublicAccessConfigurationMetadata'
  BlockPublicAccessConfigurationMetadata ->
  -- | 'responseStatus'
  Lude.Int ->
  GetBlockPublicAccessConfigurationResponse
mkGetBlockPublicAccessConfigurationResponse
  pBlockPublicAccessConfiguration_
  pBlockPublicAccessConfigurationMetadata_
  pResponseStatus_ =
    GetBlockPublicAccessConfigurationResponse'
      { blockPublicAccessConfiguration =
          pBlockPublicAccessConfiguration_,
        blockPublicAccessConfigurationMetadata =
          pBlockPublicAccessConfigurationMetadata_,
        responseStatus = pResponseStatus_
      }

-- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating the block public access configuration to remove the exception.
--
-- /Note:/ Consider using 'blockPublicAccessConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpacrsBlockPublicAccessConfiguration :: Lens.Lens' GetBlockPublicAccessConfigurationResponse BlockPublicAccessConfiguration
gbpacrsBlockPublicAccessConfiguration = Lens.lens (blockPublicAccessConfiguration :: GetBlockPublicAccessConfigurationResponse -> BlockPublicAccessConfiguration) (\s a -> s {blockPublicAccessConfiguration = a} :: GetBlockPublicAccessConfigurationResponse)
{-# DEPRECATED gbpacrsBlockPublicAccessConfiguration "Use generic-lens or generic-optics with 'blockPublicAccessConfiguration' instead." #-}

-- | Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
--
-- /Note:/ Consider using 'blockPublicAccessConfigurationMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpacrsBlockPublicAccessConfigurationMetadata :: Lens.Lens' GetBlockPublicAccessConfigurationResponse BlockPublicAccessConfigurationMetadata
gbpacrsBlockPublicAccessConfigurationMetadata = Lens.lens (blockPublicAccessConfigurationMetadata :: GetBlockPublicAccessConfigurationResponse -> BlockPublicAccessConfigurationMetadata) (\s a -> s {blockPublicAccessConfigurationMetadata = a} :: GetBlockPublicAccessConfigurationResponse)
{-# DEPRECATED gbpacrsBlockPublicAccessConfigurationMetadata "Use generic-lens or generic-optics with 'blockPublicAccessConfigurationMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpacrsResponseStatus :: Lens.Lens' GetBlockPublicAccessConfigurationResponse Lude.Int
gbpacrsResponseStatus = Lens.lens (responseStatus :: GetBlockPublicAccessConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBlockPublicAccessConfigurationResponse)
{-# DEPRECATED gbpacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
