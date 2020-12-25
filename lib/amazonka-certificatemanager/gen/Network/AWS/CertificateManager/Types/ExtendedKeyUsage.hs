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
    ekuName,
    ekuOID,
  )
where

import qualified Network.AWS.CertificateManager.Types.ExtendedKeyUsageName as Types
import qualified Network.AWS.CertificateManager.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Extended Key Usage X.509 v3 extension defines one or more purposes for which the public key can be used. This is in addition to or in place of the basic purposes specified by the Key Usage extension.
--
-- /See:/ 'mkExtendedKeyUsage' smart constructor.
data ExtendedKeyUsage = ExtendedKeyUsage'
  { -- | The name of an Extended Key Usage value.
    name :: Core.Maybe Types.ExtendedKeyUsageName,
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
    oid :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExtendedKeyUsage' value with any optional fields omitted.
mkExtendedKeyUsage ::
  ExtendedKeyUsage
mkExtendedKeyUsage =
  ExtendedKeyUsage' {name = Core.Nothing, oid = Core.Nothing}

-- | The name of an Extended Key Usage value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekuName :: Lens.Lens' ExtendedKeyUsage (Core.Maybe Types.ExtendedKeyUsageName)
ekuName = Lens.field @"name"
{-# DEPRECATED ekuName "Use generic-lens or generic-optics with 'name' instead." #-}

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
-- /Note:/ Consider using 'oid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekuOID :: Lens.Lens' ExtendedKeyUsage (Core.Maybe Types.String)
ekuOID = Lens.field @"oid"
{-# DEPRECATED ekuOID "Use generic-lens or generic-optics with 'oid' instead." #-}

instance Core.FromJSON ExtendedKeyUsage where
  parseJSON =
    Core.withObject "ExtendedKeyUsage" Core.$
      \x ->
        ExtendedKeyUsage'
          Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "OID")
