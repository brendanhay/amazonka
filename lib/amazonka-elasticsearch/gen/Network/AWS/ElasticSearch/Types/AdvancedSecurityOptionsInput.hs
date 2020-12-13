{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
  ( AdvancedSecurityOptionsInput (..),

    -- * Smart constructor
    mkAdvancedSecurityOptionsInput,

    -- * Lenses
    asoiEnabled,
    asoiInternalUserDatabaseEnabled,
    asoiMasterUserOptions,
    asoiSAMLOptions,
  )
where

import Network.AWS.ElasticSearch.Types.MasterUserOptions
import Network.AWS.ElasticSearch.Types.SAMLOptionsInput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the advanced security configuration: whether advanced security is enabled, whether the internal database option is enabled, master username and password (if internal database is enabled), and master user ARN (if IAM is enabled).
--
-- /See:/ 'mkAdvancedSecurityOptionsInput' smart constructor.
data AdvancedSecurityOptionsInput = AdvancedSecurityOptionsInput'
  { -- | True if advanced security is enabled.
    enabled :: Lude.Maybe Lude.Bool,
    -- | True if the internal user database is enabled.
    internalUserDatabaseEnabled :: Lude.Maybe Lude.Bool,
    -- | Credentials for the master user: username and password, ARN, or both.
    masterUserOptions :: Lude.Maybe MasterUserOptions,
    -- | Specifies the SAML application configuration for the domain.
    sAMLOptions :: Lude.Maybe SAMLOptionsInput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdvancedSecurityOptionsInput' with the minimum fields required to make a request.
--
-- * 'enabled' - True if advanced security is enabled.
-- * 'internalUserDatabaseEnabled' - True if the internal user database is enabled.
-- * 'masterUserOptions' - Credentials for the master user: username and password, ARN, or both.
-- * 'sAMLOptions' - Specifies the SAML application configuration for the domain.
mkAdvancedSecurityOptionsInput ::
  AdvancedSecurityOptionsInput
mkAdvancedSecurityOptionsInput =
  AdvancedSecurityOptionsInput'
    { enabled = Lude.Nothing,
      internalUserDatabaseEnabled = Lude.Nothing,
      masterUserOptions = Lude.Nothing,
      sAMLOptions = Lude.Nothing
    }

-- | True if advanced security is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoiEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Lude.Maybe Lude.Bool)
asoiEnabled = Lens.lens (enabled :: AdvancedSecurityOptionsInput -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: AdvancedSecurityOptionsInput)
{-# DEPRECATED asoiEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | True if the internal user database is enabled.
--
-- /Note:/ Consider using 'internalUserDatabaseEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoiInternalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptionsInput (Lude.Maybe Lude.Bool)
asoiInternalUserDatabaseEnabled = Lens.lens (internalUserDatabaseEnabled :: AdvancedSecurityOptionsInput -> Lude.Maybe Lude.Bool) (\s a -> s {internalUserDatabaseEnabled = a} :: AdvancedSecurityOptionsInput)
{-# DEPRECATED asoiInternalUserDatabaseEnabled "Use generic-lens or generic-optics with 'internalUserDatabaseEnabled' instead." #-}

-- | Credentials for the master user: username and password, ARN, or both.
--
-- /Note:/ Consider using 'masterUserOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoiMasterUserOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Lude.Maybe MasterUserOptions)
asoiMasterUserOptions = Lens.lens (masterUserOptions :: AdvancedSecurityOptionsInput -> Lude.Maybe MasterUserOptions) (\s a -> s {masterUserOptions = a} :: AdvancedSecurityOptionsInput)
{-# DEPRECATED asoiMasterUserOptions "Use generic-lens or generic-optics with 'masterUserOptions' instead." #-}

-- | Specifies the SAML application configuration for the domain.
--
-- /Note:/ Consider using 'sAMLOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoiSAMLOptions :: Lens.Lens' AdvancedSecurityOptionsInput (Lude.Maybe SAMLOptionsInput)
asoiSAMLOptions = Lens.lens (sAMLOptions :: AdvancedSecurityOptionsInput -> Lude.Maybe SAMLOptionsInput) (\s a -> s {sAMLOptions = a} :: AdvancedSecurityOptionsInput)
{-# DEPRECATED asoiSAMLOptions "Use generic-lens or generic-optics with 'sAMLOptions' instead." #-}

instance Lude.ToJSON AdvancedSecurityOptionsInput where
  toJSON AdvancedSecurityOptionsInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("InternalUserDatabaseEnabled" Lude..=)
              Lude.<$> internalUserDatabaseEnabled,
            ("MasterUserOptions" Lude..=) Lude.<$> masterUserOptions,
            ("SAMLOptions" Lude..=) Lude.<$> sAMLOptions
          ]
      )
