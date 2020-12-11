{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.UpdateProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing protection group. A protection group is a grouping of protected resources so they can be handled as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
module Network.AWS.Shield.UpdateProtectionGroup
  ( -- * Creating a request
    UpdateProtectionGroup (..),
    mkUpdateProtectionGroup,

    -- ** Request lenses
    upgResourceType,
    upgMembers,
    upgProtectionGroupId,
    upgAggregation,
    upgPattern,

    -- * Destructuring the response
    UpdateProtectionGroupResponse (..),
    mkUpdateProtectionGroupResponse,

    -- ** Response lenses
    upgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkUpdateProtectionGroup' smart constructor.
data UpdateProtectionGroup = UpdateProtectionGroup'
  { resourceType ::
      Lude.Maybe ProtectedResourceType,
    members :: Lude.Maybe [Lude.Text],
    protectionGroupId :: Lude.Text,
    aggregation :: ProtectionGroupAggregation,
    pattern' :: ProtectionGroupPattern
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProtectionGroup' with the minimum fields required to make a request.
--
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
--
--
-- * 'members' - The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
-- * 'pattern'' - The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
-- * 'protectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
-- * 'resourceType' - The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
mkUpdateProtectionGroup ::
  -- | 'protectionGroupId'
  Lude.Text ->
  -- | 'aggregation'
  ProtectionGroupAggregation ->
  -- | 'pattern''
  ProtectionGroupPattern ->
  UpdateProtectionGroup
mkUpdateProtectionGroup pProtectionGroupId_ pAggregation_ pPattern_ =
  UpdateProtectionGroup'
    { resourceType = Lude.Nothing,
      members = Lude.Nothing,
      protectionGroupId = pProtectionGroupId_,
      aggregation = pAggregation_,
      pattern' = pPattern_
    }

-- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgResourceType :: Lens.Lens' UpdateProtectionGroup (Lude.Maybe ProtectedResourceType)
upgResourceType = Lens.lens (resourceType :: UpdateProtectionGroup -> Lude.Maybe ProtectedResourceType) (\s a -> s {resourceType = a} :: UpdateProtectionGroup)
{-# DEPRECATED upgResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgMembers :: Lens.Lens' UpdateProtectionGroup (Lude.Maybe [Lude.Text])
upgMembers = Lens.lens (members :: UpdateProtectionGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {members = a} :: UpdateProtectionGroup)
{-# DEPRECATED upgMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgProtectionGroupId :: Lens.Lens' UpdateProtectionGroup Lude.Text
upgProtectionGroupId = Lens.lens (protectionGroupId :: UpdateProtectionGroup -> Lude.Text) (\s a -> s {protectionGroupId = a} :: UpdateProtectionGroup)
{-# DEPRECATED upgProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

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
upgAggregation :: Lens.Lens' UpdateProtectionGroup ProtectionGroupAggregation
upgAggregation = Lens.lens (aggregation :: UpdateProtectionGroup -> ProtectionGroupAggregation) (\s a -> s {aggregation = a} :: UpdateProtectionGroup)
{-# DEPRECATED upgAggregation "Use generic-lens or generic-optics with 'aggregation' instead." #-}

-- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgPattern :: Lens.Lens' UpdateProtectionGroup ProtectionGroupPattern
upgPattern = Lens.lens (pattern' :: UpdateProtectionGroup -> ProtectionGroupPattern) (\s a -> s {pattern' = a} :: UpdateProtectionGroup)
{-# DEPRECATED upgPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

instance Lude.AWSRequest UpdateProtectionGroup where
  type Rs UpdateProtectionGroup = UpdateProtectionGroupResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateProtectionGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProtectionGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.UpdateProtectionGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProtectionGroup where
  toJSON UpdateProtectionGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            ("Members" Lude..=) Lude.<$> members,
            Lude.Just ("ProtectionGroupId" Lude..= protectionGroupId),
            Lude.Just ("Aggregation" Lude..= aggregation),
            Lude.Just ("Pattern" Lude..= pattern')
          ]
      )

instance Lude.ToPath UpdateProtectionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProtectionGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProtectionGroupResponse' smart constructor.
newtype UpdateProtectionGroupResponse = UpdateProtectionGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProtectionGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateProtectionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProtectionGroupResponse
mkUpdateProtectionGroupResponse pResponseStatus_ =
  UpdateProtectionGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgrsResponseStatus :: Lens.Lens' UpdateProtectionGroupResponse Lude.Int
upgrsResponseStatus = Lens.lens (responseStatus :: UpdateProtectionGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProtectionGroupResponse)
{-# DEPRECATED upgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
