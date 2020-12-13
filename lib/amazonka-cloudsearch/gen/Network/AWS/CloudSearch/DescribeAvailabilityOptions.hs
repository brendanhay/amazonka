{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeAvailabilityOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the availability options configured for a domain. By default, shows the configuration with any pending changes. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeAvailabilityOptions
  ( -- * Creating a request
    DescribeAvailabilityOptions (..),
    mkDescribeAvailabilityOptions,

    -- ** Request lenses
    daoDeployed,
    daoDomainName,

    -- * Destructuring the response
    DescribeAvailabilityOptionsResponse (..),
    mkDescribeAvailabilityOptionsResponse,

    -- ** Response lenses
    daorsAvailabilityOptions,
    daorsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeAvailabilityOptions' @ operation. Specifies the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the Deployed option to @true@ .
--
-- /See:/ 'mkDescribeAvailabilityOptions' smart constructor.
data DescribeAvailabilityOptions = DescribeAvailabilityOptions'
  { -- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
    deployed :: Lude.Maybe Lude.Bool,
    -- | The name of the domain you want to describe.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAvailabilityOptions' with the minimum fields required to make a request.
--
-- * 'deployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
-- * 'domainName' - The name of the domain you want to describe.
mkDescribeAvailabilityOptions ::
  -- | 'domainName'
  Lude.Text ->
  DescribeAvailabilityOptions
mkDescribeAvailabilityOptions pDomainName_ =
  DescribeAvailabilityOptions'
    { deployed = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoDeployed :: Lens.Lens' DescribeAvailabilityOptions (Lude.Maybe Lude.Bool)
daoDeployed = Lens.lens (deployed :: DescribeAvailabilityOptions -> Lude.Maybe Lude.Bool) (\s a -> s {deployed = a} :: DescribeAvailabilityOptions)
{-# DEPRECATED daoDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoDomainName :: Lens.Lens' DescribeAvailabilityOptions Lude.Text
daoDomainName = Lens.lens (domainName :: DescribeAvailabilityOptions -> Lude.Text) (\s a -> s {domainName = a} :: DescribeAvailabilityOptions)
{-# DEPRECATED daoDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeAvailabilityOptions where
  type
    Rs DescribeAvailabilityOptions =
      DescribeAvailabilityOptionsResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DescribeAvailabilityOptionsResult"
      ( \s h x ->
          DescribeAvailabilityOptionsResponse'
            Lude.<$> (x Lude..@? "AvailabilityOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAvailabilityOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAvailabilityOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAvailabilityOptions where
  toQuery DescribeAvailabilityOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeAvailabilityOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "Deployed" Lude.=: deployed,
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DescribeAvailabilityOptions@ request. Indicates whether or not the Multi-AZ option is enabled for the domain specified in the request.
--
-- /See:/ 'mkDescribeAvailabilityOptionsResponse' smart constructor.
data DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse'
  { -- | The availability options configured for the domain. Indicates whether Multi-AZ is enabled for the domain.
    availabilityOptions :: Lude.Maybe AvailabilityOptionsStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAvailabilityOptionsResponse' with the minimum fields required to make a request.
--
-- * 'availabilityOptions' - The availability options configured for the domain. Indicates whether Multi-AZ is enabled for the domain.
-- * 'responseStatus' - The response status code.
mkDescribeAvailabilityOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAvailabilityOptionsResponse
mkDescribeAvailabilityOptionsResponse pResponseStatus_ =
  DescribeAvailabilityOptionsResponse'
    { availabilityOptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The availability options configured for the domain. Indicates whether Multi-AZ is enabled for the domain.
--
-- /Note:/ Consider using 'availabilityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorsAvailabilityOptions :: Lens.Lens' DescribeAvailabilityOptionsResponse (Lude.Maybe AvailabilityOptionsStatus)
daorsAvailabilityOptions = Lens.lens (availabilityOptions :: DescribeAvailabilityOptionsResponse -> Lude.Maybe AvailabilityOptionsStatus) (\s a -> s {availabilityOptions = a} :: DescribeAvailabilityOptionsResponse)
{-# DEPRECATED daorsAvailabilityOptions "Use generic-lens or generic-optics with 'availabilityOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorsResponseStatus :: Lens.Lens' DescribeAvailabilityOptionsResponse Lude.Int
daorsResponseStatus = Lens.lens (responseStatus :: DescribeAvailabilityOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAvailabilityOptionsResponse)
{-# DEPRECATED daorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
