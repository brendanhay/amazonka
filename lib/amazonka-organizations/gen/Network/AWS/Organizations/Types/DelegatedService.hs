-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.DelegatedService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.DelegatedService
  ( DelegatedService (..),

    -- * Smart constructor
    mkDelegatedService,

    -- * Lenses
    dsServicePrincipal,
    dsDelegationEnabledDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the AWS service for which the account is a delegated administrator.
--
-- /See:/ 'mkDelegatedService' smart constructor.
data DelegatedService = DelegatedService'
  { servicePrincipal ::
      Lude.Maybe Lude.Text,
    delegationEnabledDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DelegatedService' with the minimum fields required to make a request.
--
-- * 'delegationEnabledDate' - The date that the account became a delegated administrator for this service.
-- * 'servicePrincipal' - The name of a service that can request an operation for the specified service. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
mkDelegatedService ::
  DelegatedService
mkDelegatedService =
  DelegatedService'
    { servicePrincipal = Lude.Nothing,
      delegationEnabledDate = Lude.Nothing
    }

-- | The name of a service that can request an operation for the specified service. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServicePrincipal :: Lens.Lens' DelegatedService (Lude.Maybe Lude.Text)
dsServicePrincipal = Lens.lens (servicePrincipal :: DelegatedService -> Lude.Maybe Lude.Text) (\s a -> s {servicePrincipal = a} :: DelegatedService)
{-# DEPRECATED dsServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

-- | The date that the account became a delegated administrator for this service.
--
-- /Note:/ Consider using 'delegationEnabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDelegationEnabledDate :: Lens.Lens' DelegatedService (Lude.Maybe Lude.Timestamp)
dsDelegationEnabledDate = Lens.lens (delegationEnabledDate :: DelegatedService -> Lude.Maybe Lude.Timestamp) (\s a -> s {delegationEnabledDate = a} :: DelegatedService)
{-# DEPRECATED dsDelegationEnabledDate "Use generic-lens or generic-optics with 'delegationEnabledDate' instead." #-}

instance Lude.FromJSON DelegatedService where
  parseJSON =
    Lude.withObject
      "DelegatedService"
      ( \x ->
          DelegatedService'
            Lude.<$> (x Lude..:? "ServicePrincipal")
            Lude.<*> (x Lude..:? "DelegationEnabledDate")
      )
