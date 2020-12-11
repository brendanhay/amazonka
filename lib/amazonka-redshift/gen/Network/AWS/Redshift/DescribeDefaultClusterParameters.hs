{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter settings for the specified parameter group family.
--
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeDefaultClusterParameters
  ( -- * Creating a request
    DescribeDefaultClusterParameters (..),
    mkDescribeDefaultClusterParameters,

    -- ** Request lenses
    ddcpMarker,
    ddcpMaxRecords,
    ddcpParameterGroupFamily,

    -- * Destructuring the response
    DescribeDefaultClusterParametersResponse (..),
    mkDescribeDefaultClusterParametersResponse,

    -- ** Response lenses
    ddcprsResponseStatus,
    ddcprsDefaultClusterParameters,
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
-- /See:/ 'mkDescribeDefaultClusterParameters' smart constructor.
data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters'
  { marker ::
      Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    parameterGroupFamily ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDefaultClusterParameters' with the minimum fields required to make a request.
--
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeDefaultClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'parameterGroupFamily' - The name of the cluster parameter group family.
mkDescribeDefaultClusterParameters ::
  -- | 'parameterGroupFamily'
  Lude.Text ->
  DescribeDefaultClusterParameters
mkDescribeDefaultClusterParameters pParameterGroupFamily_ =
  DescribeDefaultClusterParameters'
    { marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      parameterGroupFamily = pParameterGroupFamily_
    }

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeDefaultClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpMarker :: Lens.Lens' DescribeDefaultClusterParameters (Lude.Maybe Lude.Text)
ddcpMarker = Lens.lens (marker :: DescribeDefaultClusterParameters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDefaultClusterParameters)
{-# DEPRECATED ddcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpMaxRecords :: Lens.Lens' DescribeDefaultClusterParameters (Lude.Maybe Lude.Int)
ddcpMaxRecords = Lens.lens (maxRecords :: DescribeDefaultClusterParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDefaultClusterParameters)
{-# DEPRECATED ddcpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the cluster parameter group family.
--
-- /Note:/ Consider using 'parameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpParameterGroupFamily :: Lens.Lens' DescribeDefaultClusterParameters Lude.Text
ddcpParameterGroupFamily = Lens.lens (parameterGroupFamily :: DescribeDefaultClusterParameters -> Lude.Text) (\s a -> s {parameterGroupFamily = a} :: DescribeDefaultClusterParameters)
{-# DEPRECATED ddcpParameterGroupFamily "Use generic-lens or generic-optics with 'parameterGroupFamily' instead." #-}

instance Page.AWSPager DescribeDefaultClusterParameters where
  page rq rs
    | Page.stop
        ( rs
            Lens.^? ddcprsDefaultClusterParameters Lude.. dcpMarker Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Page.stop
        ( rs
            Lens.^? ddcprsDefaultClusterParameters
              Lude.. dcpParameters
              Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcpMarker
          Lens..~ rs
          Lens.^? ddcprsDefaultClusterParameters Lude.. dcpMarker Lude.. Lens._Just

instance Lude.AWSRequest DescribeDefaultClusterParameters where
  type
    Rs DescribeDefaultClusterParameters =
      DescribeDefaultClusterParametersResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeDefaultClusterParametersResult"
      ( \s h x ->
          DescribeDefaultClusterParametersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "DefaultClusterParameters")
      )

instance Lude.ToHeaders DescribeDefaultClusterParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDefaultClusterParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDefaultClusterParameters where
  toQuery DescribeDefaultClusterParameters' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDefaultClusterParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ParameterGroupFamily" Lude.=: parameterGroupFamily
      ]

-- | /See:/ 'mkDescribeDefaultClusterParametersResponse' smart constructor.
data DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse'
  { responseStatus ::
      Lude.Int,
    defaultClusterParameters ::
      DefaultClusterParameters
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDefaultClusterParametersResponse' with the minimum fields required to make a request.
--
-- * 'defaultClusterParameters' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeDefaultClusterParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'defaultClusterParameters'
  DefaultClusterParameters ->
  DescribeDefaultClusterParametersResponse
mkDescribeDefaultClusterParametersResponse
  pResponseStatus_
  pDefaultClusterParameters_ =
    DescribeDefaultClusterParametersResponse'
      { responseStatus =
          pResponseStatus_,
        defaultClusterParameters = pDefaultClusterParameters_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcprsResponseStatus :: Lens.Lens' DescribeDefaultClusterParametersResponse Lude.Int
ddcprsResponseStatus = Lens.lens (responseStatus :: DescribeDefaultClusterParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDefaultClusterParametersResponse)
{-# DEPRECATED ddcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'defaultClusterParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcprsDefaultClusterParameters :: Lens.Lens' DescribeDefaultClusterParametersResponse DefaultClusterParameters
ddcprsDefaultClusterParameters = Lens.lens (defaultClusterParameters :: DescribeDefaultClusterParametersResponse -> DefaultClusterParameters) (\s a -> s {defaultClusterParameters = a} :: DescribeDefaultClusterParametersResponse)
{-# DEPRECATED ddcprsDefaultClusterParameters "Use generic-lens or generic-optics with 'defaultClusterParameters' instead." #-}
