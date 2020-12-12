{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EnabledServicePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EnabledServicePrincipal
  ( EnabledServicePrincipal (..),

    -- * Smart constructor
    mkEnabledServicePrincipal,

    -- * Lenses
    espServicePrincipal,
    espDateEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains details of a service principal that represents an AWS service that is enabled to integrate with AWS Organizations.
--
-- /See:/ 'mkEnabledServicePrincipal' smart constructor.
data EnabledServicePrincipal = EnabledServicePrincipal'
  { servicePrincipal ::
      Lude.Maybe Lude.Text,
    dateEnabled :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnabledServicePrincipal' with the minimum fields required to make a request.
--
-- * 'dateEnabled' - The date that the service principal was enabled for integration with AWS Organizations.
-- * 'servicePrincipal' - The name of the service principal. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
mkEnabledServicePrincipal ::
  EnabledServicePrincipal
mkEnabledServicePrincipal =
  EnabledServicePrincipal'
    { servicePrincipal = Lude.Nothing,
      dateEnabled = Lude.Nothing
    }

-- | The name of the service principal. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
espServicePrincipal :: Lens.Lens' EnabledServicePrincipal (Lude.Maybe Lude.Text)
espServicePrincipal = Lens.lens (servicePrincipal :: EnabledServicePrincipal -> Lude.Maybe Lude.Text) (\s a -> s {servicePrincipal = a} :: EnabledServicePrincipal)
{-# DEPRECATED espServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

-- | The date that the service principal was enabled for integration with AWS Organizations.
--
-- /Note:/ Consider using 'dateEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
espDateEnabled :: Lens.Lens' EnabledServicePrincipal (Lude.Maybe Lude.Timestamp)
espDateEnabled = Lens.lens (dateEnabled :: EnabledServicePrincipal -> Lude.Maybe Lude.Timestamp) (\s a -> s {dateEnabled = a} :: EnabledServicePrincipal)
{-# DEPRECATED espDateEnabled "Use generic-lens or generic-optics with 'dateEnabled' instead." #-}

instance Lude.FromJSON EnabledServicePrincipal where
  parseJSON =
    Lude.withObject
      "EnabledServicePrincipal"
      ( \x ->
          EnabledServicePrincipal'
            Lude.<$> (x Lude..:? "ServicePrincipal")
            Lude.<*> (x Lude..:? "DateEnabled")
      )
