{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
  ( ClientCertificateRevocationListStatus (..)
  -- * Smart constructor
  , mkClientCertificateRevocationListStatus
  -- * Lenses
  , ccrlsCode
  , ccrlsMessage
  ) where

import qualified Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a client certificate revocation list.
--
-- /See:/ 'mkClientCertificateRevocationListStatus' smart constructor.
data ClientCertificateRevocationListStatus = ClientCertificateRevocationListStatus'
  { code :: Core.Maybe Types.ClientCertificateRevocationListStatusCode
    -- ^ The state of the client certificate revocation list.
  , message :: Core.Maybe Core.Text
    -- ^ A message about the status of the client certificate revocation list, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientCertificateRevocationListStatus' value with any optional fields omitted.
mkClientCertificateRevocationListStatus
    :: ClientCertificateRevocationListStatus
mkClientCertificateRevocationListStatus
  = ClientCertificateRevocationListStatus'{code = Core.Nothing,
                                           message = Core.Nothing}

-- | The state of the client certificate revocation list.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrlsCode :: Lens.Lens' ClientCertificateRevocationListStatus (Core.Maybe Types.ClientCertificateRevocationListStatusCode)
ccrlsCode = Lens.field @"code"
{-# INLINEABLE ccrlsCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A message about the status of the client certificate revocation list, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrlsMessage :: Lens.Lens' ClientCertificateRevocationListStatus (Core.Maybe Core.Text)
ccrlsMessage = Lens.field @"message"
{-# INLINEABLE ccrlsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML ClientCertificateRevocationListStatus where
        parseXML x
          = ClientCertificateRevocationListStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
