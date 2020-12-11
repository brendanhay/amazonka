{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetGroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource query associated with the specified resource group.
module Network.AWS.ResourceGroups.GetGroupQuery
  ( -- * Creating a request
    GetGroupQuery (..),
    mkGetGroupQuery,

    -- ** Request lenses
    ggqGroup,
    ggqGroupName,

    -- * Destructuring the response
    GetGroupQueryResponse (..),
    mkGetGroupQueryResponse,

    -- ** Response lenses
    ggqrsGroupQuery,
    ggqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroupQuery' smart constructor.
data GetGroupQuery = GetGroupQuery'
  { group :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupQuery' with the minimum fields required to make a request.
--
-- * 'group' - The name or the ARN of the resource group to query.
-- * 'groupName' - Don't use this parameter. Use @Group@ instead.
mkGetGroupQuery ::
  GetGroupQuery
mkGetGroupQuery =
  GetGroupQuery' {group = Lude.Nothing, groupName = Lude.Nothing}

-- | The name or the ARN of the resource group to query.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggqGroup :: Lens.Lens' GetGroupQuery (Lude.Maybe Lude.Text)
ggqGroup = Lens.lens (group :: GetGroupQuery -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: GetGroupQuery)
{-# DEPRECATED ggqGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggqGroupName :: Lens.Lens' GetGroupQuery (Lude.Maybe Lude.Text)
ggqGroupName = Lens.lens (groupName :: GetGroupQuery -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GetGroupQuery)
{-# DEPRECATED ggqGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest GetGroupQuery where
  type Rs GetGroupQuery = GetGroupQueryResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupQueryResponse'
            Lude.<$> (x Lude..?> "GroupQuery") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroupQuery where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetGroupQuery where
  toJSON GetGroupQuery' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Group" Lude..=) Lude.<$> group,
            ("GroupName" Lude..=) Lude.<$> groupName
          ]
      )

instance Lude.ToPath GetGroupQuery where
  toPath = Lude.const "/get-group-query"

instance Lude.ToQuery GetGroupQuery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupQueryResponse' smart constructor.
data GetGroupQueryResponse = GetGroupQueryResponse'
  { groupQuery ::
      Lude.Maybe GroupQuery,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupQueryResponse' with the minimum fields required to make a request.
--
-- * 'groupQuery' - The resource query associated with the specified group.
-- * 'responseStatus' - The response status code.
mkGetGroupQueryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupQueryResponse
mkGetGroupQueryResponse pResponseStatus_ =
  GetGroupQueryResponse'
    { groupQuery = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The resource query associated with the specified group.
--
-- /Note:/ Consider using 'groupQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggqrsGroupQuery :: Lens.Lens' GetGroupQueryResponse (Lude.Maybe GroupQuery)
ggqrsGroupQuery = Lens.lens (groupQuery :: GetGroupQueryResponse -> Lude.Maybe GroupQuery) (\s a -> s {groupQuery = a} :: GetGroupQueryResponse)
{-# DEPRECATED ggqrsGroupQuery "Use generic-lens or generic-optics with 'groupQuery' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggqrsResponseStatus :: Lens.Lens' GetGroupQueryResponse Lude.Int
ggqrsResponseStatus = Lens.lens (responseStatus :: GetGroupQueryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupQueryResponse)
{-# DEPRECATED ggqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
