{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.OrganizationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.OrganizationSummary
  ( OrganizationSummary (..),

    -- * Smart constructor
    mkOrganizationSummary,

    -- * Lenses
    osState,
    osAlias,
    osDefaultMailDomain,
    osErrorMessage,
    osOrganizationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The representation of an organization.
--
-- /See:/ 'mkOrganizationSummary' smart constructor.
data OrganizationSummary = OrganizationSummary'
  { -- | The state associated with the organization.
    state :: Lude.Maybe Lude.Text,
    -- | The alias associated with the organization.
    alias :: Lude.Maybe Lude.Text,
    -- | The default email domain associated with the organization.
    defaultMailDomain :: Lude.Maybe Lude.Text,
    -- | The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
    errorMessage :: Lude.Maybe Lude.Text,
    -- | The identifier associated with the organization.
    organizationId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationSummary' with the minimum fields required to make a request.
--
-- * 'state' - The state associated with the organization.
-- * 'alias' - The alias associated with the organization.
-- * 'defaultMailDomain' - The default email domain associated with the organization.
-- * 'errorMessage' - The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
-- * 'organizationId' - The identifier associated with the organization.
mkOrganizationSummary ::
  OrganizationSummary
mkOrganizationSummary =
  OrganizationSummary'
    { state = Lude.Nothing,
      alias = Lude.Nothing,
      defaultMailDomain = Lude.Nothing,
      errorMessage = Lude.Nothing,
      organizationId = Lude.Nothing
    }

-- | The state associated with the organization.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osState :: Lens.Lens' OrganizationSummary (Lude.Maybe Lude.Text)
osState = Lens.lens (state :: OrganizationSummary -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: OrganizationSummary)
{-# DEPRECATED osState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The alias associated with the organization.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAlias :: Lens.Lens' OrganizationSummary (Lude.Maybe Lude.Text)
osAlias = Lens.lens (alias :: OrganizationSummary -> Lude.Maybe Lude.Text) (\s a -> s {alias = a} :: OrganizationSummary)
{-# DEPRECATED osAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The default email domain associated with the organization.
--
-- /Note:/ Consider using 'defaultMailDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDefaultMailDomain :: Lens.Lens' OrganizationSummary (Lude.Maybe Lude.Text)
osDefaultMailDomain = Lens.lens (defaultMailDomain :: OrganizationSummary -> Lude.Maybe Lude.Text) (\s a -> s {defaultMailDomain = a} :: OrganizationSummary)
{-# DEPRECATED osDefaultMailDomain "Use generic-lens or generic-optics with 'defaultMailDomain' instead." #-}

-- | The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osErrorMessage :: Lens.Lens' OrganizationSummary (Lude.Maybe Lude.Text)
osErrorMessage = Lens.lens (errorMessage :: OrganizationSummary -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: OrganizationSummary)
{-# DEPRECATED osErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The identifier associated with the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOrganizationId :: Lens.Lens' OrganizationSummary (Lude.Maybe Lude.Text)
osOrganizationId = Lens.lens (organizationId :: OrganizationSummary -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: OrganizationSummary)
{-# DEPRECATED osOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.FromJSON OrganizationSummary where
  parseJSON =
    Lude.withObject
      "OrganizationSummary"
      ( \x ->
          OrganizationSummary'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Alias")
            Lude.<*> (x Lude..:? "DefaultMailDomain")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "OrganizationId")
      )
