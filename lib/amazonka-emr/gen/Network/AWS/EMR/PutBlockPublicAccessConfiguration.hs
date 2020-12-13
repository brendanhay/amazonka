{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.PutBlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an Amazon EMR block public access configuration for your AWS account in the current Region. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/configure-block-public-access.html Configure Block Public Access for Amazon EMR> in the /Amazon EMR Management Guide/ .
module Network.AWS.EMR.PutBlockPublicAccessConfiguration
  ( -- * Creating a request
    PutBlockPublicAccessConfiguration (..),
    mkPutBlockPublicAccessConfiguration,

    -- ** Request lenses
    pbpacBlockPublicAccessConfiguration,

    -- * Destructuring the response
    PutBlockPublicAccessConfigurationResponse (..),
    mkPutBlockPublicAccessConfigurationResponse,

    -- ** Response lenses
    pbpacrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutBlockPublicAccessConfiguration' smart constructor.
newtype PutBlockPublicAccessConfiguration = PutBlockPublicAccessConfiguration'
  { -- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating @BlockPublicSecurityGroupRules@ to remove the exception.
    blockPublicAccessConfiguration :: BlockPublicAccessConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBlockPublicAccessConfiguration' with the minimum fields required to make a request.
--
-- * 'blockPublicAccessConfiguration' - A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating @BlockPublicSecurityGroupRules@ to remove the exception.
mkPutBlockPublicAccessConfiguration ::
  -- | 'blockPublicAccessConfiguration'
  BlockPublicAccessConfiguration ->
  PutBlockPublicAccessConfiguration
mkPutBlockPublicAccessConfiguration
  pBlockPublicAccessConfiguration_ =
    PutBlockPublicAccessConfiguration'
      { blockPublicAccessConfiguration =
          pBlockPublicAccessConfiguration_
      }

-- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating @BlockPublicSecurityGroupRules@ to remove the exception.
--
-- /Note:/ Consider using 'blockPublicAccessConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpacBlockPublicAccessConfiguration :: Lens.Lens' PutBlockPublicAccessConfiguration BlockPublicAccessConfiguration
pbpacBlockPublicAccessConfiguration = Lens.lens (blockPublicAccessConfiguration :: PutBlockPublicAccessConfiguration -> BlockPublicAccessConfiguration) (\s a -> s {blockPublicAccessConfiguration = a} :: PutBlockPublicAccessConfiguration)
{-# DEPRECATED pbpacBlockPublicAccessConfiguration "Use generic-lens or generic-optics with 'blockPublicAccessConfiguration' instead." #-}

instance Lude.AWSRequest PutBlockPublicAccessConfiguration where
  type
    Rs PutBlockPublicAccessConfiguration =
      PutBlockPublicAccessConfigurationResponse
  request = Req.postJSON emrService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutBlockPublicAccessConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutBlockPublicAccessConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ElasticMapReduce.PutBlockPublicAccessConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutBlockPublicAccessConfiguration where
  toJSON PutBlockPublicAccessConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "BlockPublicAccessConfiguration"
                  Lude..= blockPublicAccessConfiguration
              )
          ]
      )

instance Lude.ToPath PutBlockPublicAccessConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutBlockPublicAccessConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutBlockPublicAccessConfigurationResponse' smart constructor.
newtype PutBlockPublicAccessConfigurationResponse = PutBlockPublicAccessConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBlockPublicAccessConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutBlockPublicAccessConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutBlockPublicAccessConfigurationResponse
mkPutBlockPublicAccessConfigurationResponse pResponseStatus_ =
  PutBlockPublicAccessConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpacrsResponseStatus :: Lens.Lens' PutBlockPublicAccessConfigurationResponse Lude.Int
pbpacrsResponseStatus = Lens.lens (responseStatus :: PutBlockPublicAccessConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutBlockPublicAccessConfigurationResponse)
{-# DEPRECATED pbpacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
