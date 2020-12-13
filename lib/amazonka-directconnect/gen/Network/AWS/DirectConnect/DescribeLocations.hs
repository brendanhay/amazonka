{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS Direct Connect locations in the current AWS Region. These are the locations that can be selected when calling 'CreateConnection' or 'CreateInterconnect' .
module Network.AWS.DirectConnect.DescribeLocations
  ( -- * Creating a request
    DescribeLocations (..),
    mkDescribeLocations,

    -- * Destructuring the response
    DescribeLocationsResponse (..),
    mkDescribeLocationsResponse,

    -- ** Response lenses
    dlfrsLocations,
    dlfrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLocations' smart constructor.
data DescribeLocations = DescribeLocations'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLocations' with the minimum fields required to make a request.
mkDescribeLocations ::
  DescribeLocations
mkDescribeLocations = DescribeLocations'

instance Lude.AWSRequest DescribeLocations where
  type Rs DescribeLocations = DescribeLocationsResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLocationsResponse'
            Lude.<$> (x Lude..?> "locations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLocations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeLocations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLocations where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeLocations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLocations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLocationsResponse' smart constructor.
data DescribeLocationsResponse = DescribeLocationsResponse'
  { -- | The locations.
    locations :: Lude.Maybe [Location],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLocationsResponse' with the minimum fields required to make a request.
--
-- * 'locations' - The locations.
-- * 'responseStatus' - The response status code.
mkDescribeLocationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLocationsResponse
mkDescribeLocationsResponse pResponseStatus_ =
  DescribeLocationsResponse'
    { locations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The locations.
--
-- /Note:/ Consider using 'locations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlfrsLocations :: Lens.Lens' DescribeLocationsResponse (Lude.Maybe [Location])
dlfrsLocations = Lens.lens (locations :: DescribeLocationsResponse -> Lude.Maybe [Location]) (\s a -> s {locations = a} :: DescribeLocationsResponse)
{-# DEPRECATED dlfrsLocations "Use generic-lens or generic-optics with 'locations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlfrsResponseStatus :: Lens.Lens' DescribeLocationsResponse Lude.Int
dlfrsResponseStatus = Lens.lens (responseStatus :: DescribeLocationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLocationsResponse)
{-# DEPRECATED dlfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
