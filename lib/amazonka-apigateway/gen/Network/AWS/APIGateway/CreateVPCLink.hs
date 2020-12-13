{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateVPCLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.
module Network.AWS.APIGateway.CreateVPCLink
  ( -- * Creating a request
    CreateVPCLink (..),
    mkCreateVPCLink,

    -- ** Request lenses
    cvlTargetARNs,
    cvlName,
    cvlDescription,
    cvlTags,

    -- * Destructuring the response
    VPCLink (..),
    mkVPCLink,

    -- ** Response lenses
    vlStatus,
    vlTargetARNs,
    vlName,
    vlStatusMessage,
    vlId,
    vlDescription,
    vlTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.
--
-- /See:/ 'mkCreateVPCLink' smart constructor.
data CreateVPCLink = CreateVPCLink'
  { -- | [Required] The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
    targetARNs :: [Lude.Text],
    -- | [Required] The name used to label and identify the VPC link.
    name :: Lude.Text,
    -- | The description of the VPC link.
    description :: Lude.Maybe Lude.Text,
    -- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCLink' with the minimum fields required to make a request.
--
-- * 'targetARNs' - [Required] The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
-- * 'name' - [Required] The name used to label and identify the VPC link.
-- * 'description' - The description of the VPC link.
-- * 'tags' - The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
mkCreateVPCLink ::
  -- | 'name'
  Lude.Text ->
  CreateVPCLink
mkCreateVPCLink pName_ =
  CreateVPCLink'
    { targetARNs = Lude.mempty,
      name = pName_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | [Required] The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
--
-- /Note:/ Consider using 'targetARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlTargetARNs :: Lens.Lens' CreateVPCLink [Lude.Text]
cvlTargetARNs = Lens.lens (targetARNs :: CreateVPCLink -> [Lude.Text]) (\s a -> s {targetARNs = a} :: CreateVPCLink)
{-# DEPRECATED cvlTargetARNs "Use generic-lens or generic-optics with 'targetARNs' instead." #-}

-- | [Required] The name used to label and identify the VPC link.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlName :: Lens.Lens' CreateVPCLink Lude.Text
cvlName = Lens.lens (name :: CreateVPCLink -> Lude.Text) (\s a -> s {name = a} :: CreateVPCLink)
{-# DEPRECATED cvlName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the VPC link.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlDescription :: Lens.Lens' CreateVPCLink (Lude.Maybe Lude.Text)
cvlDescription = Lens.lens (description :: CreateVPCLink -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateVPCLink)
{-# DEPRECATED cvlDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlTags :: Lens.Lens' CreateVPCLink (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cvlTags = Lens.lens (tags :: CreateVPCLink -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateVPCLink)
{-# DEPRECATED cvlTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateVPCLink where
  type Rs CreateVPCLink = VPCLink
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateVPCLink where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateVPCLink where
  toJSON CreateVPCLink' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("targetArns" Lude..= targetARNs),
            Lude.Just ("name" Lude..= name),
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateVPCLink where
  toPath = Lude.const "/vpclinks"

instance Lude.ToQuery CreateVPCLink where
  toQuery = Lude.const Lude.mempty
