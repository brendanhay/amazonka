{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupQuery
  ( GroupQuery (..),

    -- * Smart constructor
    mkGroupQuery,

    -- * Lenses
    gqResourceQuery,
    gqGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroups.Types.ResourceQuery

-- | A mapping of a query attached to a resource group that determines the AWS resources that are members of the group.
--
-- /See:/ 'mkGroupQuery' smart constructor.
data GroupQuery = GroupQuery'
  { -- | The resource query that determines which AWS resources are members of the associated resource group.
    resourceQuery :: ResourceQuery,
    -- | The name of the resource group that is associated with the specified resource query.
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupQuery' with the minimum fields required to make a request.
--
-- * 'resourceQuery' - The resource query that determines which AWS resources are members of the associated resource group.
-- * 'groupName' - The name of the resource group that is associated with the specified resource query.
mkGroupQuery ::
  -- | 'resourceQuery'
  ResourceQuery ->
  -- | 'groupName'
  Lude.Text ->
  GroupQuery
mkGroupQuery pResourceQuery_ pGroupName_ =
  GroupQuery'
    { resourceQuery = pResourceQuery_,
      groupName = pGroupName_
    }

-- | The resource query that determines which AWS resources are members of the associated resource group.
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqResourceQuery :: Lens.Lens' GroupQuery ResourceQuery
gqResourceQuery = Lens.lens (resourceQuery :: GroupQuery -> ResourceQuery) (\s a -> s {resourceQuery = a} :: GroupQuery)
{-# DEPRECATED gqResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

-- | The name of the resource group that is associated with the specified resource query.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqGroupName :: Lens.Lens' GroupQuery Lude.Text
gqGroupName = Lens.lens (groupName :: GroupQuery -> Lude.Text) (\s a -> s {groupName = a} :: GroupQuery)
{-# DEPRECATED gqGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromJSON GroupQuery where
  parseJSON =
    Lude.withObject
      "GroupQuery"
      ( \x ->
          GroupQuery'
            Lude.<$> (x Lude..: "ResourceQuery") Lude.<*> (x Lude..: "GroupName")
      )
