{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SAMLOptionsInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SAMLOptionsInput
  ( SAMLOptionsInput (..),

    -- * Smart constructor
    mkSAMLOptionsInput,

    -- * Lenses
    samloiMasterUserName,
    samloiEnabled,
    samloiIdp,
    samloiRolesKey,
    samloiMasterBackendRole,
    samloiSessionTimeoutMinutes,
    samloiSubjectKey,
  )
where

import Network.AWS.ElasticSearch.Types.SAMLIdp
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the SAML application configuration for the domain.
--
-- /See:/ 'mkSAMLOptionsInput' smart constructor.
data SAMLOptionsInput = SAMLOptionsInput'
  { masterUserName ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    enabled :: Lude.Maybe Lude.Bool,
    idp :: Lude.Maybe SAMLIdp,
    rolesKey :: Lude.Maybe Lude.Text,
    masterBackendRole :: Lude.Maybe Lude.Text,
    sessionTimeoutMinutes :: Lude.Maybe Lude.Int,
    subjectKey :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SAMLOptionsInput' with the minimum fields required to make a request.
--
-- * 'enabled' - True if SAML is enabled.
-- * 'idp' - Specifies the SAML Identity Provider's information.
-- * 'masterBackendRole' - The backend role to which the SAML master user is mapped to.
-- * 'masterUserName' - The SAML master username, which is stored in the Amazon Elasticsearch Service domain's internal database.
-- * 'rolesKey' - The key to use for matching the SAML Roles attribute.
-- * 'sessionTimeoutMinutes' - The duration, in minutes, after which a user session becomes inactive. Acceptable values are between 1 and 1440, and the default value is 60.
-- * 'subjectKey' - The key to use for matching the SAML Subject attribute.
mkSAMLOptionsInput ::
  SAMLOptionsInput
mkSAMLOptionsInput =
  SAMLOptionsInput'
    { masterUserName = Lude.Nothing,
      enabled = Lude.Nothing,
      idp = Lude.Nothing,
      rolesKey = Lude.Nothing,
      masterBackendRole = Lude.Nothing,
      sessionTimeoutMinutes = Lude.Nothing,
      subjectKey = Lude.Nothing
    }

-- | The SAML master username, which is stored in the Amazon Elasticsearch Service domain's internal database.
--
-- /Note:/ Consider using 'masterUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiMasterUserName :: Lens.Lens' SAMLOptionsInput (Lude.Maybe (Lude.Sensitive Lude.Text))
samloiMasterUserName = Lens.lens (masterUserName :: SAMLOptionsInput -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {masterUserName = a} :: SAMLOptionsInput)
{-# DEPRECATED samloiMasterUserName "Use generic-lens or generic-optics with 'masterUserName' instead." #-}

-- | True if SAML is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiEnabled :: Lens.Lens' SAMLOptionsInput (Lude.Maybe Lude.Bool)
samloiEnabled = Lens.lens (enabled :: SAMLOptionsInput -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: SAMLOptionsInput)
{-# DEPRECATED samloiEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies the SAML Identity Provider's information.
--
-- /Note:/ Consider using 'idp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiIdp :: Lens.Lens' SAMLOptionsInput (Lude.Maybe SAMLIdp)
samloiIdp = Lens.lens (idp :: SAMLOptionsInput -> Lude.Maybe SAMLIdp) (\s a -> s {idp = a} :: SAMLOptionsInput)
{-# DEPRECATED samloiIdp "Use generic-lens or generic-optics with 'idp' instead." #-}

-- | The key to use for matching the SAML Roles attribute.
--
-- /Note:/ Consider using 'rolesKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiRolesKey :: Lens.Lens' SAMLOptionsInput (Lude.Maybe Lude.Text)
samloiRolesKey = Lens.lens (rolesKey :: SAMLOptionsInput -> Lude.Maybe Lude.Text) (\s a -> s {rolesKey = a} :: SAMLOptionsInput)
{-# DEPRECATED samloiRolesKey "Use generic-lens or generic-optics with 'rolesKey' instead." #-}

-- | The backend role to which the SAML master user is mapped to.
--
-- /Note:/ Consider using 'masterBackendRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiMasterBackendRole :: Lens.Lens' SAMLOptionsInput (Lude.Maybe Lude.Text)
samloiMasterBackendRole = Lens.lens (masterBackendRole :: SAMLOptionsInput -> Lude.Maybe Lude.Text) (\s a -> s {masterBackendRole = a} :: SAMLOptionsInput)
{-# DEPRECATED samloiMasterBackendRole "Use generic-lens or generic-optics with 'masterBackendRole' instead." #-}

-- | The duration, in minutes, after which a user session becomes inactive. Acceptable values are between 1 and 1440, and the default value is 60.
--
-- /Note:/ Consider using 'sessionTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiSessionTimeoutMinutes :: Lens.Lens' SAMLOptionsInput (Lude.Maybe Lude.Int)
samloiSessionTimeoutMinutes = Lens.lens (sessionTimeoutMinutes :: SAMLOptionsInput -> Lude.Maybe Lude.Int) (\s a -> s {sessionTimeoutMinutes = a} :: SAMLOptionsInput)
{-# DEPRECATED samloiSessionTimeoutMinutes "Use generic-lens or generic-optics with 'sessionTimeoutMinutes' instead." #-}

-- | The key to use for matching the SAML Subject attribute.
--
-- /Note:/ Consider using 'subjectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiSubjectKey :: Lens.Lens' SAMLOptionsInput (Lude.Maybe Lude.Text)
samloiSubjectKey = Lens.lens (subjectKey :: SAMLOptionsInput -> Lude.Maybe Lude.Text) (\s a -> s {subjectKey = a} :: SAMLOptionsInput)
{-# DEPRECATED samloiSubjectKey "Use generic-lens or generic-optics with 'subjectKey' instead." #-}

instance Lude.ToJSON SAMLOptionsInput where
  toJSON SAMLOptionsInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MasterUserName" Lude..=) Lude.<$> masterUserName,
            ("Enabled" Lude..=) Lude.<$> enabled,
            ("Idp" Lude..=) Lude.<$> idp,
            ("RolesKey" Lude..=) Lude.<$> rolesKey,
            ("MasterBackendRole" Lude..=) Lude.<$> masterBackendRole,
            ("SessionTimeoutMinutes" Lude..=) Lude.<$> sessionTimeoutMinutes,
            ("SubjectKey" Lude..=) Lude.<$> subjectKey
          ]
      )
