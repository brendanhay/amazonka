{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.UpdateGroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource query of a group.
module Network.AWS.ResourceGroups.UpdateGroupQuery
  ( -- * Creating a request
    UpdateGroupQuery (..),
    mkUpdateGroupQuery,

    -- ** Request lenses
    ugqGroup,
    ugqResourceQuery,
    ugqGroupName,

    -- * Destructuring the response
    UpdateGroupQueryResponse (..),
    mkUpdateGroupQueryResponse,

    -- ** Response lenses
    ugqrsGroupQuery,
    ugqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGroupQuery' smart constructor.
data UpdateGroupQuery = UpdateGroupQuery'
  { -- | The name or the ARN of the resource group to query.
    group :: Lude.Maybe Lude.Text,
    -- | The resource query to determine which AWS resources are members of this resource group.
    resourceQuery :: ResourceQuery,
    -- | Don't use this parameter. Use @Group@ instead.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupQuery' with the minimum fields required to make a request.
--
-- * 'group' - The name or the ARN of the resource group to query.
-- * 'resourceQuery' - The resource query to determine which AWS resources are members of this resource group.
-- * 'groupName' - Don't use this parameter. Use @Group@ instead.
mkUpdateGroupQuery ::
  -- | 'resourceQuery'
  ResourceQuery ->
  UpdateGroupQuery
mkUpdateGroupQuery pResourceQuery_ =
  UpdateGroupQuery'
    { group = Lude.Nothing,
      resourceQuery = pResourceQuery_,
      groupName = Lude.Nothing
    }

-- | The name or the ARN of the resource group to query.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqGroup :: Lens.Lens' UpdateGroupQuery (Lude.Maybe Lude.Text)
ugqGroup = Lens.lens (group :: UpdateGroupQuery -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: UpdateGroupQuery)
{-# DEPRECATED ugqGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The resource query to determine which AWS resources are members of this resource group.
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqResourceQuery :: Lens.Lens' UpdateGroupQuery ResourceQuery
ugqResourceQuery = Lens.lens (resourceQuery :: UpdateGroupQuery -> ResourceQuery) (\s a -> s {resourceQuery = a} :: UpdateGroupQuery)
{-# DEPRECATED ugqResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqGroupName :: Lens.Lens' UpdateGroupQuery (Lude.Maybe Lude.Text)
ugqGroupName = Lens.lens (groupName :: UpdateGroupQuery -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: UpdateGroupQuery)
{-# DEPRECATED ugqGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest UpdateGroupQuery where
  type Rs UpdateGroupQuery = UpdateGroupQueryResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGroupQueryResponse'
            Lude.<$> (x Lude..?> "GroupQuery") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGroupQuery where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateGroupQuery where
  toJSON UpdateGroupQuery' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Group" Lude..=) Lude.<$> group,
            Lude.Just ("ResourceQuery" Lude..= resourceQuery),
            ("GroupName" Lude..=) Lude.<$> groupName
          ]
      )

instance Lude.ToPath UpdateGroupQuery where
  toPath = Lude.const "/update-group-query"

instance Lude.ToQuery UpdateGroupQuery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGroupQueryResponse' smart constructor.
data UpdateGroupQueryResponse = UpdateGroupQueryResponse'
  { -- | The updated resource query associated with the resource group after the update.
    groupQuery :: Lude.Maybe GroupQuery,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupQueryResponse' with the minimum fields required to make a request.
--
-- * 'groupQuery' - The updated resource query associated with the resource group after the update.
-- * 'responseStatus' - The response status code.
mkUpdateGroupQueryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGroupQueryResponse
mkUpdateGroupQueryResponse pResponseStatus_ =
  UpdateGroupQueryResponse'
    { groupQuery = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated resource query associated with the resource group after the update.
--
-- /Note:/ Consider using 'groupQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqrsGroupQuery :: Lens.Lens' UpdateGroupQueryResponse (Lude.Maybe GroupQuery)
ugqrsGroupQuery = Lens.lens (groupQuery :: UpdateGroupQueryResponse -> Lude.Maybe GroupQuery) (\s a -> s {groupQuery = a} :: UpdateGroupQueryResponse)
{-# DEPRECATED ugqrsGroupQuery "Use generic-lens or generic-optics with 'groupQuery' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugqrsResponseStatus :: Lens.Lens' UpdateGroupQueryResponse Lude.Int
ugqrsResponseStatus = Lens.lens (responseStatus :: UpdateGroupQueryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGroupQueryResponse)
{-# DEPRECATED ugqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
