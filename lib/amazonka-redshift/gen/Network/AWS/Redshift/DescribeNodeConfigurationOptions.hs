{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeNodeConfigurationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns properties of possible node configurations such as node type, number of nodes, and disk usage for the specified action type.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeNodeConfigurationOptions
  ( -- * Creating a request
    DescribeNodeConfigurationOptions (..),
    mkDescribeNodeConfigurationOptions,

    -- ** Request lenses
    dncoSnapshotIdentifier,
    dncoFilters,
    dncoClusterIdentifier,
    dncoMarker,
    dncoMaxRecords,
    dncoOwnerAccount,
    dncoActionType,

    -- * Destructuring the response
    DescribeNodeConfigurationOptionsResponse (..),
    mkDescribeNodeConfigurationOptionsResponse,

    -- ** Response lenses
    dncorsNodeConfigurationOptionList,
    dncorsMarker,
    dncorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeNodeConfigurationOptions' smart constructor.
data DescribeNodeConfigurationOptions = DescribeNodeConfigurationOptions'
  { -- | The identifier of the snapshot to evaluate for possible node configurations.
    snapshotIdentifier :: Lude.Maybe Lude.Text,
    -- | A set of name, operator, and value items to filter the results.
    filters :: Lude.Maybe [NodeConfigurationOptionsFilter],
    -- | The identifier of the cluster to evaluate for possible node configurations.
    clusterIdentifier :: Lude.Maybe Lude.Text,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeNodeConfigurationOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @500@
    -- Constraints: minimum 100, maximum 500.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
    ownerAccount :: Lude.Maybe Lude.Text,
    -- | The action type to evaluate for possible node configurations. Specify "restore-cluster" to get configuration combinations based on an existing snapshot. Specify "recommend-node-config" to get configuration recommendations based on an existing cluster or snapshot. Specify "resize-cluster" to get configuration combinations for elastic resize based on an existing cluster.
    actionType :: ActionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNodeConfigurationOptions' with the minimum fields required to make a request.
--
-- * 'snapshotIdentifier' - The identifier of the snapshot to evaluate for possible node configurations.
-- * 'filters' - A set of name, operator, and value items to filter the results.
-- * 'clusterIdentifier' - The identifier of the cluster to evaluate for possible node configurations.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeNodeConfigurationOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @500@
-- Constraints: minimum 100, maximum 500.
-- * 'ownerAccount' - The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
-- * 'actionType' - The action type to evaluate for possible node configurations. Specify "restore-cluster" to get configuration combinations based on an existing snapshot. Specify "recommend-node-config" to get configuration recommendations based on an existing cluster or snapshot. Specify "resize-cluster" to get configuration combinations for elastic resize based on an existing cluster.
mkDescribeNodeConfigurationOptions ::
  -- | 'actionType'
  ActionType ->
  DescribeNodeConfigurationOptions
mkDescribeNodeConfigurationOptions pActionType_ =
  DescribeNodeConfigurationOptions'
    { snapshotIdentifier =
        Lude.Nothing,
      filters = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      ownerAccount = Lude.Nothing,
      actionType = pActionType_
    }

-- | The identifier of the snapshot to evaluate for possible node configurations.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoSnapshotIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Lude.Maybe Lude.Text)
dncoSnapshotIdentifier = Lens.lens (snapshotIdentifier :: DescribeNodeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {snapshotIdentifier = a} :: DescribeNodeConfigurationOptions)
{-# DEPRECATED dncoSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | A set of name, operator, and value items to filter the results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoFilters :: Lens.Lens' DescribeNodeConfigurationOptions (Lude.Maybe [NodeConfigurationOptionsFilter])
dncoFilters = Lens.lens (filters :: DescribeNodeConfigurationOptions -> Lude.Maybe [NodeConfigurationOptionsFilter]) (\s a -> s {filters = a} :: DescribeNodeConfigurationOptions)
{-# DEPRECATED dncoFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The identifier of the cluster to evaluate for possible node configurations.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoClusterIdentifier :: Lens.Lens' DescribeNodeConfigurationOptions (Lude.Maybe Lude.Text)
dncoClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeNodeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeNodeConfigurationOptions)
{-# DEPRECATED dncoClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeNodeConfigurationOptions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoMarker :: Lens.Lens' DescribeNodeConfigurationOptions (Lude.Maybe Lude.Text)
dncoMarker = Lens.lens (marker :: DescribeNodeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeNodeConfigurationOptions)
{-# DEPRECATED dncoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @500@
-- Constraints: minimum 100, maximum 500.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoMaxRecords :: Lens.Lens' DescribeNodeConfigurationOptions (Lude.Maybe Lude.Int)
dncoMaxRecords = Lens.lens (maxRecords :: DescribeNodeConfigurationOptions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeNodeConfigurationOptions)
{-# DEPRECATED dncoMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The AWS customer account used to create or copy the snapshot. Required if you are restoring a snapshot you do not own, optional if you own the snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoOwnerAccount :: Lens.Lens' DescribeNodeConfigurationOptions (Lude.Maybe Lude.Text)
dncoOwnerAccount = Lens.lens (ownerAccount :: DescribeNodeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: DescribeNodeConfigurationOptions)
{-# DEPRECATED dncoOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The action type to evaluate for possible node configurations. Specify "restore-cluster" to get configuration combinations based on an existing snapshot. Specify "recommend-node-config" to get configuration recommendations based on an existing cluster or snapshot. Specify "resize-cluster" to get configuration combinations for elastic resize based on an existing cluster.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncoActionType :: Lens.Lens' DescribeNodeConfigurationOptions ActionType
dncoActionType = Lens.lens (actionType :: DescribeNodeConfigurationOptions -> ActionType) (\s a -> s {actionType = a} :: DescribeNodeConfigurationOptions)
{-# DEPRECATED dncoActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

instance Page.AWSPager DescribeNodeConfigurationOptions where
  page rq rs
    | Page.stop (rs Lens.^. dncorsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dncorsNodeConfigurationOptionList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dncoMarker Lens..~ rs Lens.^. dncorsMarker

instance Lude.AWSRequest DescribeNodeConfigurationOptions where
  type
    Rs DescribeNodeConfigurationOptions =
      DescribeNodeConfigurationOptionsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeNodeConfigurationOptionsResult"
      ( \s h x ->
          DescribeNodeConfigurationOptionsResponse'
            Lude.<$> ( x Lude..@? "NodeConfigurationOptionList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "NodeConfigurationOption")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNodeConfigurationOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeNodeConfigurationOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNodeConfigurationOptions where
  toQuery DescribeNodeConfigurationOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeNodeConfigurationOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SnapshotIdentifier" Lude.=: snapshotIdentifier,
        "Filter"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "NodeConfigurationOptionsFilter"
                Lude.<$> filters
            ),
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "OwnerAccount" Lude.=: ownerAccount,
        "ActionType" Lude.=: actionType
      ]

-- | /See:/ 'mkDescribeNodeConfigurationOptionsResponse' smart constructor.
data DescribeNodeConfigurationOptionsResponse = DescribeNodeConfigurationOptionsResponse'
  { -- | A list of valid node configurations.
    nodeConfigurationOptionList :: Lude.Maybe [NodeConfigurationOption],
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNodeConfigurationOptionsResponse' with the minimum fields required to make a request.
--
-- * 'nodeConfigurationOptionList' - A list of valid node configurations.
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'responseStatus' - The response status code.
mkDescribeNodeConfigurationOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNodeConfigurationOptionsResponse
mkDescribeNodeConfigurationOptionsResponse pResponseStatus_ =
  DescribeNodeConfigurationOptionsResponse'
    { nodeConfigurationOptionList =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of valid node configurations.
--
-- /Note:/ Consider using 'nodeConfigurationOptionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorsNodeConfigurationOptionList :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Lude.Maybe [NodeConfigurationOption])
dncorsNodeConfigurationOptionList = Lens.lens (nodeConfigurationOptionList :: DescribeNodeConfigurationOptionsResponse -> Lude.Maybe [NodeConfigurationOption]) (\s a -> s {nodeConfigurationOptionList = a} :: DescribeNodeConfigurationOptionsResponse)
{-# DEPRECATED dncorsNodeConfigurationOptionList "Use generic-lens or generic-optics with 'nodeConfigurationOptionList' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorsMarker :: Lens.Lens' DescribeNodeConfigurationOptionsResponse (Lude.Maybe Lude.Text)
dncorsMarker = Lens.lens (marker :: DescribeNodeConfigurationOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeNodeConfigurationOptionsResponse)
{-# DEPRECATED dncorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncorsResponseStatus :: Lens.Lens' DescribeNodeConfigurationOptionsResponse Lude.Int
dncorsResponseStatus = Lens.lens (responseStatus :: DescribeNodeConfigurationOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNodeConfigurationOptionsResponse)
{-# DEPRECATED dncorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
