{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to failover the primary region to a selected secondary region. The selected secondary region will become primary, and all other clusters will become secondary.
module Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
  ( -- * Creating a request
    FailoverGlobalReplicationGroup (..),
    mkFailoverGlobalReplicationGroup,

    -- ** Request lenses
    fgrgPrimaryRegion,
    fgrgPrimaryReplicationGroupId,
    fgrgGlobalReplicationGroupId,

    -- * Destructuring the response
    FailoverGlobalReplicationGroupResponse (..),
    mkFailoverGlobalReplicationGroupResponse,

    -- ** Response lenses
    fgrgrsGlobalReplicationGroup,
    fgrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkFailoverGlobalReplicationGroup' smart constructor.
data FailoverGlobalReplicationGroup = FailoverGlobalReplicationGroup'
  { -- | The AWS region of the primary cluster of the Global Datastore
    primaryRegion :: Lude.Text,
    -- | The name of the primary replication group
    primaryReplicationGroupId :: Lude.Text,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailoverGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'primaryRegion' - The AWS region of the primary cluster of the Global Datastore
-- * 'primaryReplicationGroupId' - The name of the primary replication group
-- * 'globalReplicationGroupId' - The name of the Global Datastore
mkFailoverGlobalReplicationGroup ::
  -- | 'primaryRegion'
  Lude.Text ->
  -- | 'primaryReplicationGroupId'
  Lude.Text ->
  -- | 'globalReplicationGroupId'
  Lude.Text ->
  FailoverGlobalReplicationGroup
mkFailoverGlobalReplicationGroup
  pPrimaryRegion_
  pPrimaryReplicationGroupId_
  pGlobalReplicationGroupId_ =
    FailoverGlobalReplicationGroup'
      { primaryRegion = pPrimaryRegion_,
        primaryReplicationGroupId = pPrimaryReplicationGroupId_,
        globalReplicationGroupId = pGlobalReplicationGroupId_
      }

-- | The AWS region of the primary cluster of the Global Datastore
--
-- /Note:/ Consider using 'primaryRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgPrimaryRegion :: Lens.Lens' FailoverGlobalReplicationGroup Lude.Text
fgrgPrimaryRegion = Lens.lens (primaryRegion :: FailoverGlobalReplicationGroup -> Lude.Text) (\s a -> s {primaryRegion = a} :: FailoverGlobalReplicationGroup)
{-# DEPRECATED fgrgPrimaryRegion "Use generic-lens or generic-optics with 'primaryRegion' instead." #-}

-- | The name of the primary replication group
--
-- /Note:/ Consider using 'primaryReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgPrimaryReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Lude.Text
fgrgPrimaryReplicationGroupId = Lens.lens (primaryReplicationGroupId :: FailoverGlobalReplicationGroup -> Lude.Text) (\s a -> s {primaryReplicationGroupId = a} :: FailoverGlobalReplicationGroup)
{-# DEPRECATED fgrgPrimaryReplicationGroupId "Use generic-lens or generic-optics with 'primaryReplicationGroupId' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgGlobalReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Lude.Text
fgrgGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: FailoverGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: FailoverGlobalReplicationGroup)
{-# DEPRECATED fgrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

instance Lude.AWSRequest FailoverGlobalReplicationGroup where
  type
    Rs FailoverGlobalReplicationGroup =
      FailoverGlobalReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "FailoverGlobalReplicationGroupResult"
      ( \s h x ->
          FailoverGlobalReplicationGroupResponse'
            Lude.<$> (x Lude..@? "GlobalReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders FailoverGlobalReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath FailoverGlobalReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery FailoverGlobalReplicationGroup where
  toQuery FailoverGlobalReplicationGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("FailoverGlobalReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "PrimaryRegion" Lude.=: primaryRegion,
        "PrimaryReplicationGroupId" Lude.=: primaryReplicationGroupId,
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId
      ]

-- | /See:/ 'mkFailoverGlobalReplicationGroupResponse' smart constructor.
data FailoverGlobalReplicationGroupResponse = FailoverGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Lude.Maybe GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailoverGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' -
-- * 'responseStatus' - The response status code.
mkFailoverGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  FailoverGlobalReplicationGroupResponse
mkFailoverGlobalReplicationGroupResponse pResponseStatus_ =
  FailoverGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgrsGlobalReplicationGroup :: Lens.Lens' FailoverGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
fgrgrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: FailoverGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: FailoverGlobalReplicationGroupResponse)
{-# DEPRECATED fgrgrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgrsResponseStatus :: Lens.Lens' FailoverGlobalReplicationGroupResponse Lude.Int
fgrgrsResponseStatus = Lens.lens (responseStatus :: FailoverGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: FailoverGlobalReplicationGroupResponse)
{-# DEPRECATED fgrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
