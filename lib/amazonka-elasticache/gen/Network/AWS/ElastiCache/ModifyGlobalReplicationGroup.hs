{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a Global Datastore.
module Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
  ( -- * Creating a request
    ModifyGlobalReplicationGroup (..),
    mkModifyGlobalReplicationGroup,

    -- ** Request lenses
    mgrgAutomaticFailoverEnabled,
    mgrgEngineVersion,
    mgrgCacheNodeType,
    mgrgGlobalReplicationGroupId,
    mgrgApplyImmediately,
    mgrgGlobalReplicationGroupDescription,

    -- * Destructuring the response
    ModifyGlobalReplicationGroupResponse (..),
    mkModifyGlobalReplicationGroupResponse,

    -- ** Response lenses
    mgrgrsGlobalReplicationGroup,
    mgrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyGlobalReplicationGroup' smart constructor.
data ModifyGlobalReplicationGroup = ModifyGlobalReplicationGroup'
  { -- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
    automaticFailoverEnabled :: Lude.Maybe Lude.Bool,
    -- | The upgraded version of the cache engine to be run on the clusters in the Global Datastore.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | A valid cache node type that you want to scale this Global Datastore to.
    cacheNodeType :: Lude.Maybe Lude.Text,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Lude.Text,
    -- | This parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible. Modifications to Global Replication Groups cannot be requested to be applied in PreferredMaintenceWindow.
    applyImmediately :: Lude.Bool,
    -- | A description of the Global Datastore
    globalReplicationGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'automaticFailoverEnabled' - Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
-- * 'engineVersion' - The upgraded version of the cache engine to be run on the clusters in the Global Datastore.
-- * 'cacheNodeType' - A valid cache node type that you want to scale this Global Datastore to.
-- * 'globalReplicationGroupId' - The name of the Global Datastore
-- * 'applyImmediately' - This parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible. Modifications to Global Replication Groups cannot be requested to be applied in PreferredMaintenceWindow.
-- * 'globalReplicationGroupDescription' - A description of the Global Datastore
mkModifyGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Lude.Text ->
  -- | 'applyImmediately'
  Lude.Bool ->
  ModifyGlobalReplicationGroup
mkModifyGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pApplyImmediately_ =
    ModifyGlobalReplicationGroup'
      { automaticFailoverEnabled =
          Lude.Nothing,
        engineVersion = Lude.Nothing,
        cacheNodeType = Lude.Nothing,
        globalReplicationGroupId = pGlobalReplicationGroupId_,
        applyImmediately = pApplyImmediately_,
        globalReplicationGroupDescription = Lude.Nothing
      }

-- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
--
-- /Note:/ Consider using 'automaticFailoverEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgAutomaticFailoverEnabled :: Lens.Lens' ModifyGlobalReplicationGroup (Lude.Maybe Lude.Bool)
mgrgAutomaticFailoverEnabled = Lens.lens (automaticFailoverEnabled :: ModifyGlobalReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {automaticFailoverEnabled = a} :: ModifyGlobalReplicationGroup)
{-# DEPRECATED mgrgAutomaticFailoverEnabled "Use generic-lens or generic-optics with 'automaticFailoverEnabled' instead." #-}

-- | The upgraded version of the cache engine to be run on the clusters in the Global Datastore.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgEngineVersion :: Lens.Lens' ModifyGlobalReplicationGroup (Lude.Maybe Lude.Text)
mgrgEngineVersion = Lens.lens (engineVersion :: ModifyGlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ModifyGlobalReplicationGroup)
{-# DEPRECATED mgrgEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A valid cache node type that you want to scale this Global Datastore to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgCacheNodeType :: Lens.Lens' ModifyGlobalReplicationGroup (Lude.Maybe Lude.Text)
mgrgCacheNodeType = Lens.lens (cacheNodeType :: ModifyGlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: ModifyGlobalReplicationGroup)
{-# DEPRECATED mgrgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgGlobalReplicationGroupId :: Lens.Lens' ModifyGlobalReplicationGroup Lude.Text
mgrgGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: ModifyGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: ModifyGlobalReplicationGroup)
{-# DEPRECATED mgrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | This parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible. Modifications to Global Replication Groups cannot be requested to be applied in PreferredMaintenceWindow.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgApplyImmediately :: Lens.Lens' ModifyGlobalReplicationGroup Lude.Bool
mgrgApplyImmediately = Lens.lens (applyImmediately :: ModifyGlobalReplicationGroup -> Lude.Bool) (\s a -> s {applyImmediately = a} :: ModifyGlobalReplicationGroup)
{-# DEPRECATED mgrgApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | A description of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgGlobalReplicationGroupDescription :: Lens.Lens' ModifyGlobalReplicationGroup (Lude.Maybe Lude.Text)
mgrgGlobalReplicationGroupDescription = Lens.lens (globalReplicationGroupDescription :: ModifyGlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {globalReplicationGroupDescription = a} :: ModifyGlobalReplicationGroup)
{-# DEPRECATED mgrgGlobalReplicationGroupDescription "Use generic-lens or generic-optics with 'globalReplicationGroupDescription' instead." #-}

instance Lude.AWSRequest ModifyGlobalReplicationGroup where
  type
    Rs ModifyGlobalReplicationGroup =
      ModifyGlobalReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ModifyGlobalReplicationGroupResult"
      ( \s h x ->
          ModifyGlobalReplicationGroupResponse'
            Lude.<$> (x Lude..@? "GlobalReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyGlobalReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyGlobalReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyGlobalReplicationGroup where
  toQuery ModifyGlobalReplicationGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyGlobalReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "AutomaticFailoverEnabled" Lude.=: automaticFailoverEnabled,
        "EngineVersion" Lude.=: engineVersion,
        "CacheNodeType" Lude.=: cacheNodeType,
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId,
        "ApplyImmediately" Lude.=: applyImmediately,
        "GlobalReplicationGroupDescription"
          Lude.=: globalReplicationGroupDescription
      ]

-- | /See:/ 'mkModifyGlobalReplicationGroupResponse' smart constructor.
data ModifyGlobalReplicationGroupResponse = ModifyGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Lude.Maybe GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' -
-- * 'responseStatus' - The response status code.
mkModifyGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyGlobalReplicationGroupResponse
mkModifyGlobalReplicationGroupResponse pResponseStatus_ =
  ModifyGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgrsGlobalReplicationGroup :: Lens.Lens' ModifyGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
mgrgrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: ModifyGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: ModifyGlobalReplicationGroupResponse)
{-# DEPRECATED mgrgrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgrsResponseStatus :: Lens.Lens' ModifyGlobalReplicationGroupResponse Lude.Int
mgrgrsResponseStatus = Lens.lens (responseStatus :: ModifyGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyGlobalReplicationGroupResponse)
{-# DEPRECATED mgrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
