{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.CreateProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a grouping of protected resources so they can be handled as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
module Network.AWS.Shield.CreateProtectionGroup
  ( -- * Creating a request
    CreateProtectionGroup (..),
    mkCreateProtectionGroup,

    -- ** Request lenses
    cpgResourceType,
    cpgPattern,
    cpgMembers,
    cpgProtectionGroupId,
    cpgAggregation,

    -- * Destructuring the response
    CreateProtectionGroupResponse (..),
    mkCreateProtectionGroupResponse,

    -- ** Response lenses
    cpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkCreateProtectionGroup' smart constructor.
data CreateProtectionGroup = CreateProtectionGroup'
  { -- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. Newly protected resources of this type are automatically added to the group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
    resourceType :: Lude.Maybe ProtectedResourceType,
    -- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
    pattern' :: ProtectionGroupPattern,
    -- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
    members :: Lude.Maybe [Lude.Text],
    -- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
    protectionGroupId :: Lude.Text,
    -- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
    --
    --
    --     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
    --
    --
    --     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
    --
    --
    --     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
    aggregation :: ProtectionGroupAggregation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProtectionGroup' with the minimum fields required to make a request.
--
-- * 'resourceType' - The resource type to include in the protection group. All protected resources of this type are included in the protection group. Newly protected resources of this type are automatically added to the group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
-- * 'pattern'' - The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
-- * 'members' - The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
-- * 'protectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
-- * 'aggregation' - Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
--
--
--     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
--
--
--     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
--
--
--     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
mkCreateProtectionGroup ::
  -- | 'pattern''
  ProtectionGroupPattern ->
  -- | 'protectionGroupId'
  Lude.Text ->
  -- | 'aggregation'
  ProtectionGroupAggregation ->
  CreateProtectionGroup
mkCreateProtectionGroup pPattern_ pProtectionGroupId_ pAggregation_ =
  CreateProtectionGroup'
    { resourceType = Lude.Nothing,
      pattern' = pPattern_,
      members = Lude.Nothing,
      protectionGroupId = pProtectionGroupId_,
      aggregation = pAggregation_
    }

-- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. Newly protected resources of this type are automatically added to the group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgResourceType :: Lens.Lens' CreateProtectionGroup (Lude.Maybe ProtectedResourceType)
cpgResourceType = Lens.lens (resourceType :: CreateProtectionGroup -> Lude.Maybe ProtectedResourceType) (\s a -> s {resourceType = a} :: CreateProtectionGroup)
{-# DEPRECATED cpgResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgPattern :: Lens.Lens' CreateProtectionGroup ProtectionGroupPattern
cpgPattern = Lens.lens (pattern' :: CreateProtectionGroup -> ProtectionGroupPattern) (\s a -> s {pattern' = a} :: CreateProtectionGroup)
{-# DEPRECATED cpgPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

-- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgMembers :: Lens.Lens' CreateProtectionGroup (Lude.Maybe [Lude.Text])
cpgMembers = Lens.lens (members :: CreateProtectionGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {members = a} :: CreateProtectionGroup)
{-# DEPRECATED cpgMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgProtectionGroupId :: Lens.Lens' CreateProtectionGroup Lude.Text
cpgProtectionGroupId = Lens.lens (protectionGroupId :: CreateProtectionGroup -> Lude.Text) (\s a -> s {protectionGroupId = a} :: CreateProtectionGroup)
{-# DEPRECATED cpgProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

-- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
--
--
--     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
--
--
--     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
--
--
--     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
--
--
-- /Note:/ Consider using 'aggregation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgAggregation :: Lens.Lens' CreateProtectionGroup ProtectionGroupAggregation
cpgAggregation = Lens.lens (aggregation :: CreateProtectionGroup -> ProtectionGroupAggregation) (\s a -> s {aggregation = a} :: CreateProtectionGroup)
{-# DEPRECATED cpgAggregation "Use generic-lens or generic-optics with 'aggregation' instead." #-}

instance Lude.AWSRequest CreateProtectionGroup where
  type Rs CreateProtectionGroup = CreateProtectionGroupResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateProtectionGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProtectionGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.CreateProtectionGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProtectionGroup where
  toJSON CreateProtectionGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            Lude.Just ("Pattern" Lude..= pattern'),
            ("Members" Lude..=) Lude.<$> members,
            Lude.Just ("ProtectionGroupId" Lude..= protectionGroupId),
            Lude.Just ("Aggregation" Lude..= aggregation)
          ]
      )

instance Lude.ToPath CreateProtectionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProtectionGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProtectionGroupResponse' smart constructor.
newtype CreateProtectionGroupResponse = CreateProtectionGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProtectionGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateProtectionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProtectionGroupResponse
mkCreateProtectionGroupResponse pResponseStatus_ =
  CreateProtectionGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrsResponseStatus :: Lens.Lens' CreateProtectionGroupResponse Lude.Int
cpgrsResponseStatus = Lens.lens (responseStatus :: CreateProtectionGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProtectionGroupResponse)
{-# DEPRECATED cpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
