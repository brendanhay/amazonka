{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
  ( AdvancedSecurityOptions (..),

    -- * Smart constructor
    mkAdvancedSecurityOptions,

    -- * Lenses
    asoEnabled,
    asoInternalUserDatabaseEnabled,
    asoSAMLOptions,
  )
where

import Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the advanced security configuration: whether advanced security is enabled, whether the internal database option is enabled.
--
-- /See:/ 'mkAdvancedSecurityOptions' smart constructor.
data AdvancedSecurityOptions = AdvancedSecurityOptions'
  { enabled ::
      Lude.Maybe Lude.Bool,
    internalUserDatabaseEnabled ::
      Lude.Maybe Lude.Bool,
    sAMLOptions :: Lude.Maybe SAMLOptionsOutput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdvancedSecurityOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - True if advanced security is enabled.
-- * 'internalUserDatabaseEnabled' - True if the internal user database is enabled.
-- * 'sAMLOptions' - Describes the SAML application configured for a domain.
mkAdvancedSecurityOptions ::
  AdvancedSecurityOptions
mkAdvancedSecurityOptions =
  AdvancedSecurityOptions'
    { enabled = Lude.Nothing,
      internalUserDatabaseEnabled = Lude.Nothing,
      sAMLOptions = Lude.Nothing
    }

-- | True if advanced security is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoEnabled :: Lens.Lens' AdvancedSecurityOptions (Lude.Maybe Lude.Bool)
asoEnabled = Lens.lens (enabled :: AdvancedSecurityOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: AdvancedSecurityOptions)
{-# DEPRECATED asoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | True if the internal user database is enabled.
--
-- /Note:/ Consider using 'internalUserDatabaseEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoInternalUserDatabaseEnabled :: Lens.Lens' AdvancedSecurityOptions (Lude.Maybe Lude.Bool)
asoInternalUserDatabaseEnabled = Lens.lens (internalUserDatabaseEnabled :: AdvancedSecurityOptions -> Lude.Maybe Lude.Bool) (\s a -> s {internalUserDatabaseEnabled = a} :: AdvancedSecurityOptions)
{-# DEPRECATED asoInternalUserDatabaseEnabled "Use generic-lens or generic-optics with 'internalUserDatabaseEnabled' instead." #-}

-- | Describes the SAML application configured for a domain.
--
-- /Note:/ Consider using 'sAMLOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asoSAMLOptions :: Lens.Lens' AdvancedSecurityOptions (Lude.Maybe SAMLOptionsOutput)
asoSAMLOptions = Lens.lens (sAMLOptions :: AdvancedSecurityOptions -> Lude.Maybe SAMLOptionsOutput) (\s a -> s {sAMLOptions = a} :: AdvancedSecurityOptions)
{-# DEPRECATED asoSAMLOptions "Use generic-lens or generic-optics with 'sAMLOptions' instead." #-}

instance Lude.FromJSON AdvancedSecurityOptions where
  parseJSON =
    Lude.withObject
      "AdvancedSecurityOptions"
      ( \x ->
          AdvancedSecurityOptions'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "InternalUserDatabaseEnabled")
            Lude.<*> (x Lude..:? "SAMLOptions")
      )
