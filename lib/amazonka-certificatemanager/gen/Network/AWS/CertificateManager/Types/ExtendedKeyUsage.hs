{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ExtendedKeyUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ExtendedKeyUsage
  ( ExtendedKeyUsage (..),

    -- * Smart constructor
    mkExtendedKeyUsage,

    -- * Lenses
    ekuOId,
    ekuName,
  )
where

import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Extended Key Usage X.509 v3 extension defines one or more purposes for which the public key can be used. This is in addition to or in place of the basic purposes specified by the Key Usage extension.
--
-- /See:/ 'mkExtendedKeyUsage' smart constructor.
data ExtendedKeyUsage = ExtendedKeyUsage'
  { oId ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe ExtendedKeyUsageName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExtendedKeyUsage' with the minimum fields required to make a request.
--
-- * 'name' - The name of an Extended Key Usage value.
-- * 'oId' - An object identifier (OID) for the extension value. OIDs are strings of numbers separated by periods. The following OIDs are defined in RFC 3280 and RFC 5280.
--
--
--     * @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@
--
--
--     * @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@
--
--
--     * @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@
--
--
--     * @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@
--
--
--     * @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@
--
--
--     * @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@
--
--
--     * @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@
--
--
--     * @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@
--
--
--     * @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
mkExtendedKeyUsage ::
  ExtendedKeyUsage
mkExtendedKeyUsage =
  ExtendedKeyUsage' {oId = Lude.Nothing, name = Lude.Nothing}

-- | An object identifier (OID) for the extension value. OIDs are strings of numbers separated by periods. The following OIDs are defined in RFC 3280 and RFC 5280.
--
--
--     * @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@
--
--
--     * @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@
--
--
--     * @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@
--
--
--     * @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@
--
--
--     * @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@
--
--
--     * @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@
--
--
--     * @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@
--
--
--     * @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@
--
--
--     * @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
--
--
--
-- /Note:/ Consider using 'oId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekuOId :: Lens.Lens' ExtendedKeyUsage (Lude.Maybe Lude.Text)
ekuOId = Lens.lens (oId :: ExtendedKeyUsage -> Lude.Maybe Lude.Text) (\s a -> s {oId = a} :: ExtendedKeyUsage)
{-# DEPRECATED ekuOId "Use generic-lens or generic-optics with 'oId' instead." #-}

-- | The name of an Extended Key Usage value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekuName :: Lens.Lens' ExtendedKeyUsage (Lude.Maybe ExtendedKeyUsageName)
ekuName = Lens.lens (name :: ExtendedKeyUsage -> Lude.Maybe ExtendedKeyUsageName) (\s a -> s {name = a} :: ExtendedKeyUsage)
{-# DEPRECATED ekuName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ExtendedKeyUsage where
  parseJSON =
    Lude.withObject
      "ExtendedKeyUsage"
      ( \x ->
          ExtendedKeyUsage'
            Lude.<$> (x Lude..:? "OID") Lude.<*> (x Lude..:? "Name")
      )
