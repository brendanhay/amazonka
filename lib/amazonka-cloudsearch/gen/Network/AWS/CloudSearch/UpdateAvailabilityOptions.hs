{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateAvailabilityOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the availability options for a domain. Enabling the Multi-AZ option expands an Amazon CloudSearch domain to an additional Availability Zone in the same Region to increase fault tolerance in the event of a service disruption. Changes to the Multi-AZ option can take about half an hour to become active. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.UpdateAvailabilityOptions
  ( -- * Creating a request
    UpdateAvailabilityOptions (..),
    mkUpdateAvailabilityOptions,

    -- ** Request lenses
    uaoDomainName,
    uaoMultiAZ,

    -- * Destructuring the response
    UpdateAvailabilityOptionsResponse (..),
    mkUpdateAvailabilityOptionsResponse,

    -- ** Response lenses
    uaorsAvailabilityOptions,
    uaorsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'UpdateAvailabilityOptions' @ operation. Specifies the name of the domain you want to update and the Multi-AZ availability option.
--
-- /See:/ 'mkUpdateAvailabilityOptions' smart constructor.
data UpdateAvailabilityOptions = UpdateAvailabilityOptions'
  { domainName :: Lude.Text,
    -- | You expand an existing search domain to a second Availability Zone by setting the Multi-AZ option to true. Similarly, you can turn off the Multi-AZ option to downgrade the domain to a single Availability Zone by setting the Multi-AZ option to @false@ .
    multiAZ :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAvailabilityOptions' with the minimum fields required to make a request.
--
-- * 'domainName' -
-- * 'multiAZ' - You expand an existing search domain to a second Availability Zone by setting the Multi-AZ option to true. Similarly, you can turn off the Multi-AZ option to downgrade the domain to a single Availability Zone by setting the Multi-AZ option to @false@ .
mkUpdateAvailabilityOptions ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'multiAZ'
  Lude.Bool ->
  UpdateAvailabilityOptions
mkUpdateAvailabilityOptions pDomainName_ pMultiAZ_ =
  UpdateAvailabilityOptions'
    { domainName = pDomainName_,
      multiAZ = pMultiAZ_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaoDomainName :: Lens.Lens' UpdateAvailabilityOptions Lude.Text
uaoDomainName = Lens.lens (domainName :: UpdateAvailabilityOptions -> Lude.Text) (\s a -> s {domainName = a} :: UpdateAvailabilityOptions)
{-# DEPRECATED uaoDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | You expand an existing search domain to a second Availability Zone by setting the Multi-AZ option to true. Similarly, you can turn off the Multi-AZ option to downgrade the domain to a single Availability Zone by setting the Multi-AZ option to @false@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaoMultiAZ :: Lens.Lens' UpdateAvailabilityOptions Lude.Bool
uaoMultiAZ = Lens.lens (multiAZ :: UpdateAvailabilityOptions -> Lude.Bool) (\s a -> s {multiAZ = a} :: UpdateAvailabilityOptions)
{-# DEPRECATED uaoMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

instance Lude.AWSRequest UpdateAvailabilityOptions where
  type
    Rs UpdateAvailabilityOptions =
      UpdateAvailabilityOptionsResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "UpdateAvailabilityOptionsResult"
      ( \s h x ->
          UpdateAvailabilityOptionsResponse'
            Lude.<$> (x Lude..@? "AvailabilityOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAvailabilityOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateAvailabilityOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAvailabilityOptions where
  toQuery UpdateAvailabilityOptions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateAvailabilityOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "MultiAZ" Lude.=: multiAZ
      ]

-- | The result of a @UpdateAvailabilityOptions@ request. Contains the status of the domain's availability options.
--
-- /See:/ 'mkUpdateAvailabilityOptionsResponse' smart constructor.
data UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse'
  { -- | The newly-configured availability options. Indicates whether Multi-AZ is enabled for the domain.
    availabilityOptions :: Lude.Maybe AvailabilityOptionsStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAvailabilityOptionsResponse' with the minimum fields required to make a request.
--
-- * 'availabilityOptions' - The newly-configured availability options. Indicates whether Multi-AZ is enabled for the domain.
-- * 'responseStatus' - The response status code.
mkUpdateAvailabilityOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAvailabilityOptionsResponse
mkUpdateAvailabilityOptionsResponse pResponseStatus_ =
  UpdateAvailabilityOptionsResponse'
    { availabilityOptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly-configured availability options. Indicates whether Multi-AZ is enabled for the domain.
--
-- /Note:/ Consider using 'availabilityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaorsAvailabilityOptions :: Lens.Lens' UpdateAvailabilityOptionsResponse (Lude.Maybe AvailabilityOptionsStatus)
uaorsAvailabilityOptions = Lens.lens (availabilityOptions :: UpdateAvailabilityOptionsResponse -> Lude.Maybe AvailabilityOptionsStatus) (\s a -> s {availabilityOptions = a} :: UpdateAvailabilityOptionsResponse)
{-# DEPRECATED uaorsAvailabilityOptions "Use generic-lens or generic-optics with 'availabilityOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaorsResponseStatus :: Lens.Lens' UpdateAvailabilityOptionsResponse Lude.Int
uaorsResponseStatus = Lens.lens (responseStatus :: UpdateAvailabilityOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAvailabilityOptionsResponse)
{-# DEPRECATED uaorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
