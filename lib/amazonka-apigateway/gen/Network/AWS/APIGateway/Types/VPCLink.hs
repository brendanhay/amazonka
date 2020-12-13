{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.VPCLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.VPCLink
  ( VPCLink (..),

    -- * Smart constructor
    mkVPCLink,

    -- * Lenses
    vlStatus,
    vlTargetARNs,
    vlName,
    vlStatusMessage,
    vlId,
    vlDescription,
    vlTags,
  )
where

import Network.AWS.APIGateway.Types.VPCLinkStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An API Gateway VPC link for a 'RestApi' to access resources in an Amazon Virtual Private Cloud (VPC).
--
-- To enable access to a resource in an Amazon Virtual Private Cloud through Amazon API Gateway, you, as an API developer, create a 'VpcLink' resource targeted for one or more network load balancers of the VPC and then integrate an API method with a private integration that uses the 'VpcLink' . The private integration has an integration type of @HTTP@ or @HTTP_PROXY@ and has a connection type of @VPC_LINK@ . The integration uses the @connectionId@ property to identify the 'VpcLink' used.
--
--
-- /See:/ 'mkVPCLink' smart constructor.
data VPCLink = VPCLink'
  { -- | The status of the VPC link. The valid values are @AVAILABLE@ , @PENDING@ , @DELETING@ , or @FAILED@ . Deploying an API will wait if the status is @PENDING@ and will fail if the status is @DELETING@ .
    status :: Lude.Maybe VPCLinkStatus,
    -- | The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
    targetARNs :: Lude.Maybe [Lude.Text],
    -- | The name used to label and identify the VPC link.
    name :: Lude.Maybe Lude.Text,
    -- | A description about the VPC link status.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
    id :: Lude.Maybe Lude.Text,
    -- | The description of the VPC link.
    description :: Lude.Maybe Lude.Text,
    -- | The collection of tags. Each tag element is associated with a given resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCLink' with the minimum fields required to make a request.
--
-- * 'status' - The status of the VPC link. The valid values are @AVAILABLE@ , @PENDING@ , @DELETING@ , or @FAILED@ . Deploying an API will wait if the status is @PENDING@ and will fail if the status is @DELETING@ .
-- * 'targetARNs' - The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
-- * 'name' - The name used to label and identify the VPC link.
-- * 'statusMessage' - A description about the VPC link status.
-- * 'id' - The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
-- * 'description' - The description of the VPC link.
-- * 'tags' - The collection of tags. Each tag element is associated with a given resource.
mkVPCLink ::
  VPCLink
mkVPCLink =
  VPCLink'
    { status = Lude.Nothing,
      targetARNs = Lude.Nothing,
      name = Lude.Nothing,
      statusMessage = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The status of the VPC link. The valid values are @AVAILABLE@ , @PENDING@ , @DELETING@ , or @FAILED@ . Deploying an API will wait if the status is @PENDING@ and will fail if the status is @DELETING@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlStatus :: Lens.Lens' VPCLink (Lude.Maybe VPCLinkStatus)
vlStatus = Lens.lens (status :: VPCLink -> Lude.Maybe VPCLinkStatus) (\s a -> s {status = a} :: VPCLink)
{-# DEPRECATED vlStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
--
-- /Note:/ Consider using 'targetARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlTargetARNs :: Lens.Lens' VPCLink (Lude.Maybe [Lude.Text])
vlTargetARNs = Lens.lens (targetARNs :: VPCLink -> Lude.Maybe [Lude.Text]) (\s a -> s {targetARNs = a} :: VPCLink)
{-# DEPRECATED vlTargetARNs "Use generic-lens or generic-optics with 'targetARNs' instead." #-}

-- | The name used to label and identify the VPC link.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlName :: Lens.Lens' VPCLink (Lude.Maybe Lude.Text)
vlName = Lens.lens (name :: VPCLink -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: VPCLink)
{-# DEPRECATED vlName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description about the VPC link status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlStatusMessage :: Lens.Lens' VPCLink (Lude.Maybe Lude.Text)
vlStatusMessage = Lens.lens (statusMessage :: VPCLink -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: VPCLink)
{-# DEPRECATED vlStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlId :: Lens.Lens' VPCLink (Lude.Maybe Lude.Text)
vlId = Lens.lens (id :: VPCLink -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: VPCLink)
{-# DEPRECATED vlId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The description of the VPC link.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlDescription :: Lens.Lens' VPCLink (Lude.Maybe Lude.Text)
vlDescription = Lens.lens (description :: VPCLink -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: VPCLink)
{-# DEPRECATED vlDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlTags :: Lens.Lens' VPCLink (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
vlTags = Lens.lens (tags :: VPCLink -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: VPCLink)
{-# DEPRECATED vlTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON VPCLink where
  parseJSON =
    Lude.withObject
      "VPCLink"
      ( \x ->
          VPCLink'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "targetArns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "statusMessage")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
