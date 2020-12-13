{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
  ( SAMLOptionsOutput (..),

    -- * Smart constructor
    mkSAMLOptionsOutput,

    -- * Lenses
    samlooEnabled,
    samlooIdp,
    samlooRolesKey,
    samlooSessionTimeoutMinutes,
    samlooSubjectKey,
  )
where

import Network.AWS.ElasticSearch.Types.SAMLIdp
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the SAML application configured for the domain.
--
-- /See:/ 'mkSAMLOptionsOutput' smart constructor.
data SAMLOptionsOutput = SAMLOptionsOutput'
  { -- | True if SAML is enabled.
    enabled :: Lude.Maybe Lude.Bool,
    -- | Describes the SAML Identity Provider's information.
    idp :: Lude.Maybe SAMLIdp,
    -- | The key used for matching the SAML Roles attribute.
    rolesKey :: Lude.Maybe Lude.Text,
    -- | The duration, in minutes, after which a user session becomes inactive.
    sessionTimeoutMinutes :: Lude.Maybe Lude.Int,
    -- | The key used for matching the SAML Subject attribute.
    subjectKey :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SAMLOptionsOutput' with the minimum fields required to make a request.
--
-- * 'enabled' - True if SAML is enabled.
-- * 'idp' - Describes the SAML Identity Provider's information.
-- * 'rolesKey' - The key used for matching the SAML Roles attribute.
-- * 'sessionTimeoutMinutes' - The duration, in minutes, after which a user session becomes inactive.
-- * 'subjectKey' - The key used for matching the SAML Subject attribute.
mkSAMLOptionsOutput ::
  SAMLOptionsOutput
mkSAMLOptionsOutput =
  SAMLOptionsOutput'
    { enabled = Lude.Nothing,
      idp = Lude.Nothing,
      rolesKey = Lude.Nothing,
      sessionTimeoutMinutes = Lude.Nothing,
      subjectKey = Lude.Nothing
    }

-- | True if SAML is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooEnabled :: Lens.Lens' SAMLOptionsOutput (Lude.Maybe Lude.Bool)
samlooEnabled = Lens.lens (enabled :: SAMLOptionsOutput -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: SAMLOptionsOutput)
{-# DEPRECATED samlooEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Describes the SAML Identity Provider's information.
--
-- /Note:/ Consider using 'idp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooIdp :: Lens.Lens' SAMLOptionsOutput (Lude.Maybe SAMLIdp)
samlooIdp = Lens.lens (idp :: SAMLOptionsOutput -> Lude.Maybe SAMLIdp) (\s a -> s {idp = a} :: SAMLOptionsOutput)
{-# DEPRECATED samlooIdp "Use generic-lens or generic-optics with 'idp' instead." #-}

-- | The key used for matching the SAML Roles attribute.
--
-- /Note:/ Consider using 'rolesKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooRolesKey :: Lens.Lens' SAMLOptionsOutput (Lude.Maybe Lude.Text)
samlooRolesKey = Lens.lens (rolesKey :: SAMLOptionsOutput -> Lude.Maybe Lude.Text) (\s a -> s {rolesKey = a} :: SAMLOptionsOutput)
{-# DEPRECATED samlooRolesKey "Use generic-lens or generic-optics with 'rolesKey' instead." #-}

-- | The duration, in minutes, after which a user session becomes inactive.
--
-- /Note:/ Consider using 'sessionTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooSessionTimeoutMinutes :: Lens.Lens' SAMLOptionsOutput (Lude.Maybe Lude.Int)
samlooSessionTimeoutMinutes = Lens.lens (sessionTimeoutMinutes :: SAMLOptionsOutput -> Lude.Maybe Lude.Int) (\s a -> s {sessionTimeoutMinutes = a} :: SAMLOptionsOutput)
{-# DEPRECATED samlooSessionTimeoutMinutes "Use generic-lens or generic-optics with 'sessionTimeoutMinutes' instead." #-}

-- | The key used for matching the SAML Subject attribute.
--
-- /Note:/ Consider using 'subjectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooSubjectKey :: Lens.Lens' SAMLOptionsOutput (Lude.Maybe Lude.Text)
samlooSubjectKey = Lens.lens (subjectKey :: SAMLOptionsOutput -> Lude.Maybe Lude.Text) (\s a -> s {subjectKey = a} :: SAMLOptionsOutput)
{-# DEPRECATED samlooSubjectKey "Use generic-lens or generic-optics with 'subjectKey' instead." #-}

instance Lude.FromJSON SAMLOptionsOutput where
  parseJSON =
    Lude.withObject
      "SAMLOptionsOutput"
      ( \x ->
          SAMLOptionsOutput'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "Idp")
            Lude.<*> (x Lude..:? "RolesKey")
            Lude.<*> (x Lude..:? "SessionTimeoutMinutes")
            Lude.<*> (x Lude..:? "SubjectKey")
      )
