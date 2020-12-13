{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all valid regions for Amazon Lightsail. Use the @include availability zones@ parameter to also return the Availability Zones in a region.
module Network.AWS.Lightsail.GetRegions
  ( -- * Creating a request
    GetRegions (..),
    mkGetRegions,

    -- ** Request lenses
    grIncludeRelationalDatabaseAvailabilityZones,
    grIncludeAvailabilityZones,

    -- * Destructuring the response
    GetRegionsResponse (..),
    mkGetRegionsResponse,

    -- ** Response lenses
    grrsRegions,
    grrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRegions' smart constructor.
data GetRegions = GetRegions'
  { -- | A Boolean value indicating whether to also include Availability Zones for databases in your get regions request. Availability Zones are indicated with a letter (e.g., @us-east-2a@ ).
    includeRelationalDatabaseAvailabilityZones :: Lude.Maybe Lude.Bool,
    -- | A Boolean value indicating whether to also include Availability Zones in your get regions request. Availability Zones are indicated with a letter: e.g., @us-east-2a@ .
    includeAvailabilityZones :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRegions' with the minimum fields required to make a request.
--
-- * 'includeRelationalDatabaseAvailabilityZones' - A Boolean value indicating whether to also include Availability Zones for databases in your get regions request. Availability Zones are indicated with a letter (e.g., @us-east-2a@ ).
-- * 'includeAvailabilityZones' - A Boolean value indicating whether to also include Availability Zones in your get regions request. Availability Zones are indicated with a letter: e.g., @us-east-2a@ .
mkGetRegions ::
  GetRegions
mkGetRegions =
  GetRegions'
    { includeRelationalDatabaseAvailabilityZones =
        Lude.Nothing,
      includeAvailabilityZones = Lude.Nothing
    }

-- | A Boolean value indicating whether to also include Availability Zones for databases in your get regions request. Availability Zones are indicated with a letter (e.g., @us-east-2a@ ).
--
-- /Note:/ Consider using 'includeRelationalDatabaseAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grIncludeRelationalDatabaseAvailabilityZones :: Lens.Lens' GetRegions (Lude.Maybe Lude.Bool)
grIncludeRelationalDatabaseAvailabilityZones = Lens.lens (includeRelationalDatabaseAvailabilityZones :: GetRegions -> Lude.Maybe Lude.Bool) (\s a -> s {includeRelationalDatabaseAvailabilityZones = a} :: GetRegions)
{-# DEPRECATED grIncludeRelationalDatabaseAvailabilityZones "Use generic-lens or generic-optics with 'includeRelationalDatabaseAvailabilityZones' instead." #-}

-- | A Boolean value indicating whether to also include Availability Zones in your get regions request. Availability Zones are indicated with a letter: e.g., @us-east-2a@ .
--
-- /Note:/ Consider using 'includeAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grIncludeAvailabilityZones :: Lens.Lens' GetRegions (Lude.Maybe Lude.Bool)
grIncludeAvailabilityZones = Lens.lens (includeAvailabilityZones :: GetRegions -> Lude.Maybe Lude.Bool) (\s a -> s {includeAvailabilityZones = a} :: GetRegions)
{-# DEPRECATED grIncludeAvailabilityZones "Use generic-lens or generic-optics with 'includeAvailabilityZones' instead." #-}

instance Lude.AWSRequest GetRegions where
  type Rs GetRegions = GetRegionsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRegionsResponse'
            Lude.<$> (x Lude..?> "regions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRegions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetRegions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRegions where
  toJSON GetRegions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("includeRelationalDatabaseAvailabilityZones" Lude..=)
              Lude.<$> includeRelationalDatabaseAvailabilityZones,
            ("includeAvailabilityZones" Lude..=)
              Lude.<$> includeAvailabilityZones
          ]
      )

instance Lude.ToPath GetRegions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRegions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRegionsResponse' smart constructor.
data GetRegionsResponse = GetRegionsResponse'
  { -- | An array of key-value pairs containing information about your get regions request.
    regions :: Lude.Maybe [RegionInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRegionsResponse' with the minimum fields required to make a request.
--
-- * 'regions' - An array of key-value pairs containing information about your get regions request.
-- * 'responseStatus' - The response status code.
mkGetRegionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRegionsResponse
mkGetRegionsResponse pResponseStatus_ =
  GetRegionsResponse'
    { regions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs containing information about your get regions request.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRegions :: Lens.Lens' GetRegionsResponse (Lude.Maybe [RegionInfo])
grrsRegions = Lens.lens (regions :: GetRegionsResponse -> Lude.Maybe [RegionInfo]) (\s a -> s {regions = a} :: GetRegionsResponse)
{-# DEPRECATED grrsRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetRegionsResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetRegionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRegionsResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
