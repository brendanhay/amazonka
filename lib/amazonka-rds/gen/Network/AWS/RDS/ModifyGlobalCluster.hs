{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Aurora global cluster. You can change one or more database configuration parameters by specifying these parameters and the new values in the request. For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.ModifyGlobalCluster
  ( -- * Creating a request
    ModifyGlobalCluster (..),
    mkModifyGlobalCluster,

    -- ** Request lenses
    mgcDeletionProtection,
    mgcGlobalClusterIdentifier,
    mgcNewGlobalClusterIdentifier,

    -- * Destructuring the response
    ModifyGlobalClusterResponse (..),
    mkModifyGlobalClusterResponse,

    -- ** Response lenses
    mgcrsGlobalCluster,
    mgcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyGlobalCluster' smart constructor.
data ModifyGlobalCluster = ModifyGlobalCluster'
  { -- | Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when deletion protection is enabled.
    deletionProtection :: Lude.Maybe Lude.Bool,
    -- | The DB cluster identifier for the global cluster being modified. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing global database cluster.
    globalClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The new cluster identifier for the global database cluster when modifying a global database cluster. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 letters, numbers, or hyphens
    --
    --
    --     * The first character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-cluster2@
    newGlobalClusterIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyGlobalCluster' with the minimum fields required to make a request.
--
-- * 'deletionProtection' - Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when deletion protection is enabled.
-- * 'globalClusterIdentifier' - The DB cluster identifier for the global cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing global database cluster.
--
--
-- * 'newGlobalClusterIdentifier' - The new cluster identifier for the global database cluster when modifying a global database cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens
--
--
--     * The first character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-cluster2@
mkModifyGlobalCluster ::
  ModifyGlobalCluster
mkModifyGlobalCluster =
  ModifyGlobalCluster'
    { deletionProtection = Lude.Nothing,
      globalClusterIdentifier = Lude.Nothing,
      newGlobalClusterIdentifier = Lude.Nothing
    }

-- | Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when deletion protection is enabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcDeletionProtection :: Lens.Lens' ModifyGlobalCluster (Lude.Maybe Lude.Bool)
mgcDeletionProtection = Lens.lens (deletionProtection :: ModifyGlobalCluster -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: ModifyGlobalCluster)
{-# DEPRECATED mgcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The DB cluster identifier for the global cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing global database cluster.
--
--
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Lude.Maybe Lude.Text)
mgcGlobalClusterIdentifier = Lens.lens (globalClusterIdentifier :: ModifyGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {globalClusterIdentifier = a} :: ModifyGlobalCluster)
{-# DEPRECATED mgcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

-- | The new cluster identifier for the global database cluster when modifying a global database cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens
--
--
--     * The first character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-cluster2@
--
-- /Note:/ Consider using 'newGlobalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcNewGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Lude.Maybe Lude.Text)
mgcNewGlobalClusterIdentifier = Lens.lens (newGlobalClusterIdentifier :: ModifyGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {newGlobalClusterIdentifier = a} :: ModifyGlobalCluster)
{-# DEPRECATED mgcNewGlobalClusterIdentifier "Use generic-lens or generic-optics with 'newGlobalClusterIdentifier' instead." #-}

instance Lude.AWSRequest ModifyGlobalCluster where
  type Rs ModifyGlobalCluster = ModifyGlobalClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyGlobalClusterResult"
      ( \s h x ->
          ModifyGlobalClusterResponse'
            Lude.<$> (x Lude..@? "GlobalCluster")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyGlobalCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyGlobalCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyGlobalCluster where
  toQuery ModifyGlobalCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyGlobalCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DeletionProtection" Lude.=: deletionProtection,
        "GlobalClusterIdentifier" Lude.=: globalClusterIdentifier,
        "NewGlobalClusterIdentifier" Lude.=: newGlobalClusterIdentifier
      ]

-- | /See:/ 'mkModifyGlobalClusterResponse' smart constructor.
data ModifyGlobalClusterResponse = ModifyGlobalClusterResponse'
  { globalCluster :: Lude.Maybe GlobalCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyGlobalClusterResponse' with the minimum fields required to make a request.
--
-- * 'globalCluster' -
-- * 'responseStatus' - The response status code.
mkModifyGlobalClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyGlobalClusterResponse
mkModifyGlobalClusterResponse pResponseStatus_ =
  ModifyGlobalClusterResponse'
    { globalCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcrsGlobalCluster :: Lens.Lens' ModifyGlobalClusterResponse (Lude.Maybe GlobalCluster)
mgcrsGlobalCluster = Lens.lens (globalCluster :: ModifyGlobalClusterResponse -> Lude.Maybe GlobalCluster) (\s a -> s {globalCluster = a} :: ModifyGlobalClusterResponse)
{-# DEPRECATED mgcrsGlobalCluster "Use generic-lens or generic-optics with 'globalCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgcrsResponseStatus :: Lens.Lens' ModifyGlobalClusterResponse Lude.Int
mgcrsResponseStatus = Lens.lens (responseStatus :: ModifyGlobalClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyGlobalClusterResponse)
{-# DEPRECATED mgcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
