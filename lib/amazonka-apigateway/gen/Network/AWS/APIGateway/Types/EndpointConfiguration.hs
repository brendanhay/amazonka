{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.EndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.EndpointConfiguration
  ( EndpointConfiguration (..),

    -- * Smart constructor
    mkEndpointConfiguration,

    -- * Lenses
    ecTypes,
    ecVpcEndpointIds,
  )
where

import Network.AWS.APIGateway.Types.EndpointType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The endpoint configuration to indicate the types of endpoints an API ('RestApi' ) or its custom domain name ('DomainName' ) has.
--
-- /See:/ 'mkEndpointConfiguration' smart constructor.
data EndpointConfiguration = EndpointConfiguration'
  { types ::
      Lude.Maybe [EndpointType],
    vpcEndpointIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointConfiguration' with the minimum fields required to make a request.
--
-- * 'types' - A list of endpoint types of an API ('RestApi' ) or its custom domain name ('DomainName' ). For an edge-optimized API and its custom domain name, the endpoint type is @"EDGE"@ . For a regional API and its custom domain name, the endpoint type is @REGIONAL@ . For a private API, the endpoint type is @PRIVATE@ .
-- * 'vpcEndpointIds' - A list of VpcEndpointIds of an API ('RestApi' ) against which to create Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
mkEndpointConfiguration ::
  EndpointConfiguration
mkEndpointConfiguration =
  EndpointConfiguration'
    { types = Lude.Nothing,
      vpcEndpointIds = Lude.Nothing
    }

-- | A list of endpoint types of an API ('RestApi' ) or its custom domain name ('DomainName' ). For an edge-optimized API and its custom domain name, the endpoint type is @"EDGE"@ . For a regional API and its custom domain name, the endpoint type is @REGIONAL@ . For a private API, the endpoint type is @PRIVATE@ .
--
-- /Note:/ Consider using 'types' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecTypes :: Lens.Lens' EndpointConfiguration (Lude.Maybe [EndpointType])
ecTypes = Lens.lens (types :: EndpointConfiguration -> Lude.Maybe [EndpointType]) (\s a -> s {types = a} :: EndpointConfiguration)
{-# DEPRECATED ecTypes "Use generic-lens or generic-optics with 'types' instead." #-}

-- | A list of VpcEndpointIds of an API ('RestApi' ) against which to create Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecVpcEndpointIds :: Lens.Lens' EndpointConfiguration (Lude.Maybe [Lude.Text])
ecVpcEndpointIds = Lens.lens (vpcEndpointIds :: EndpointConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcEndpointIds = a} :: EndpointConfiguration)
{-# DEPRECATED ecVpcEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead." #-}

instance Lude.FromJSON EndpointConfiguration where
  parseJSON =
    Lude.withObject
      "EndpointConfiguration"
      ( \x ->
          EndpointConfiguration'
            Lude.<$> (x Lude..:? "types" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "vpcEndpointIds" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON EndpointConfiguration where
  toJSON EndpointConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("types" Lude..=) Lude.<$> types,
            ("vpcEndpointIds" Lude..=) Lude.<$> vpcEndpointIds
          ]
      )
