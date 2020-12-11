{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a detailed list of parameters contained within the specified Amazon Redshift parameter group. For each parameter the response includes information such as parameter name, description, data type, value, whether the parameter value is modifiable, and so on.
--
-- You can specify /source/ filter to retrieve parameters of only specific type. For example, to retrieve parameters that were modified by a user action such as from 'ModifyClusterParameterGroup' , you can specify /source/ equal to /user/ .
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterParameters
  ( -- * Creating a request
    DescribeClusterParameters (..),
    mkDescribeClusterParameters,

    -- ** Request lenses
    dcpsMarker,
    dcpsMaxRecords,
    dcpsSource,
    dcpsParameterGroupName,

    -- * Destructuring the response
    DescribeClusterParametersResponse (..),
    mkDescribeClusterParametersResponse,

    -- ** Response lenses
    dcprsMarker,
    dcprsParameters,
    dcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeClusterParameters' smart constructor.
data DescribeClusterParameters = DescribeClusterParameters'
  { marker ::
      Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    source :: Lude.Maybe Lude.Text,
    parameterGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterParameters' with the minimum fields required to make a request.
--
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'parameterGroupName' - The name of a cluster parameter group for which to return details.
-- * 'source' - The parameter types to return. Specify @user@ to show parameters that are different form the default. Similarly, specify @engine-default@ to show parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
-- Valid Values: @user@ | @engine-default@
mkDescribeClusterParameters ::
  -- | 'parameterGroupName'
  Lude.Text ->
  DescribeClusterParameters
mkDescribeClusterParameters pParameterGroupName_ =
  DescribeClusterParameters'
    { marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      source = Lude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsMarker :: Lens.Lens' DescribeClusterParameters (Lude.Maybe Lude.Text)
dcpsMarker = Lens.lens (marker :: DescribeClusterParameters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterParameters)
{-# DEPRECATED dcpsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsMaxRecords :: Lens.Lens' DescribeClusterParameters (Lude.Maybe Lude.Int)
dcpsMaxRecords = Lens.lens (maxRecords :: DescribeClusterParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusterParameters)
{-# DEPRECATED dcpsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The parameter types to return. Specify @user@ to show parameters that are different form the default. Similarly, specify @engine-default@ to show parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
-- Valid Values: @user@ | @engine-default@
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsSource :: Lens.Lens' DescribeClusterParameters (Lude.Maybe Lude.Text)
dcpsSource = Lens.lens (source :: DescribeClusterParameters -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: DescribeClusterParameters)
{-# DEPRECATED dcpsSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of a cluster parameter group for which to return details.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsParameterGroupName :: Lens.Lens' DescribeClusterParameters Lude.Text
dcpsParameterGroupName = Lens.lens (parameterGroupName :: DescribeClusterParameters -> Lude.Text) (\s a -> s {parameterGroupName = a} :: DescribeClusterParameters)
{-# DEPRECATED dcpsParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Page.AWSPager DescribeClusterParameters where
  page rq rs
    | Page.stop (rs Lens.^. dcprsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcprsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcpsMarker Lens..~ rs Lens.^. dcprsMarker

instance Lude.AWSRequest DescribeClusterParameters where
  type
    Rs DescribeClusterParameters =
      DescribeClusterParametersResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClusterParametersResult"
      ( \s h x ->
          DescribeClusterParametersResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Parameter")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusterParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusterParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusterParameters where
  toQuery DescribeClusterParameters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeClusterParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "Source" Lude.=: source,
        "ParameterGroupName" Lude.=: parameterGroupName
      ]

-- | Contains the output from the 'DescribeClusterParameters' action.
--
-- /See:/ 'mkDescribeClusterParametersResponse' smart constructor.
data DescribeClusterParametersResponse = DescribeClusterParametersResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    parameters ::
      Lude.Maybe [Parameter],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterParametersResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'parameters' - A list of 'Parameter' instances. Each instance lists the parameters of one cluster parameter group.
-- * 'responseStatus' - The response status code.
mkDescribeClusterParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterParametersResponse
mkDescribeClusterParametersResponse pResponseStatus_ =
  DescribeClusterParametersResponse'
    { marker = Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsMarker :: Lens.Lens' DescribeClusterParametersResponse (Lude.Maybe Lude.Text)
dcprsMarker = Lens.lens (marker :: DescribeClusterParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterParametersResponse)
{-# DEPRECATED dcprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of 'Parameter' instances. Each instance lists the parameters of one cluster parameter group.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsParameters :: Lens.Lens' DescribeClusterParametersResponse (Lude.Maybe [Parameter])
dcprsParameters = Lens.lens (parameters :: DescribeClusterParametersResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DescribeClusterParametersResponse)
{-# DEPRECATED dcprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsResponseStatus :: Lens.Lens' DescribeClusterParametersResponse Lude.Int
dcprsResponseStatus = Lens.lens (responseStatus :: DescribeClusterParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterParametersResponse)
{-# DEPRECATED dcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
