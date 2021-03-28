{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OutgoingCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.OutgoingCertificate
  ( OutgoingCertificate (..)
  -- * Smart constructor
  , mkOutgoingCertificate
  -- * Lenses
  , ocCertificateArn
  , ocCertificateId
  , ocCreationDate
  , ocTransferDate
  , ocTransferMessage
  , ocTransferredTo
  ) where

import qualified Network.AWS.IoT.Types.AwsAccountId as Types
import qualified Network.AWS.IoT.Types.CertificateArn as Types
import qualified Network.AWS.IoT.Types.CertificateId as Types
import qualified Network.AWS.IoT.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A certificate that has been transferred but not yet accepted.
--
-- /See:/ 'mkOutgoingCertificate' smart constructor.
data OutgoingCertificate = OutgoingCertificate'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The certificate ARN.
  , certificateId :: Core.Maybe Types.CertificateId
    -- ^ The certificate ID.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The certificate creation date.
  , transferDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the transfer was initiated.
  , transferMessage :: Core.Maybe Types.Message
    -- ^ The transfer message.
  , transferredTo :: Core.Maybe Types.AwsAccountId
    -- ^ The AWS account to which the transfer was made.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OutgoingCertificate' value with any optional fields omitted.
mkOutgoingCertificate
    :: OutgoingCertificate
mkOutgoingCertificate
  = OutgoingCertificate'{certificateArn = Core.Nothing,
                         certificateId = Core.Nothing, creationDate = Core.Nothing,
                         transferDate = Core.Nothing, transferMessage = Core.Nothing,
                         transferredTo = Core.Nothing}

-- | The certificate ARN.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocCertificateArn :: Lens.Lens' OutgoingCertificate (Core.Maybe Types.CertificateArn)
ocCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE ocCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The certificate ID.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocCertificateId :: Lens.Lens' OutgoingCertificate (Core.Maybe Types.CertificateId)
ocCertificateId = Lens.field @"certificateId"
{-# INLINEABLE ocCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The certificate creation date.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocCreationDate :: Lens.Lens' OutgoingCertificate (Core.Maybe Core.NominalDiffTime)
ocCreationDate = Lens.field @"creationDate"
{-# INLINEABLE ocCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The date the transfer was initiated.
--
-- /Note:/ Consider using 'transferDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTransferDate :: Lens.Lens' OutgoingCertificate (Core.Maybe Core.NominalDiffTime)
ocTransferDate = Lens.field @"transferDate"
{-# INLINEABLE ocTransferDate #-}
{-# DEPRECATED transferDate "Use generic-lens or generic-optics with 'transferDate' instead"  #-}

-- | The transfer message.
--
-- /Note:/ Consider using 'transferMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTransferMessage :: Lens.Lens' OutgoingCertificate (Core.Maybe Types.Message)
ocTransferMessage = Lens.field @"transferMessage"
{-# INLINEABLE ocTransferMessage #-}
{-# DEPRECATED transferMessage "Use generic-lens or generic-optics with 'transferMessage' instead"  #-}

-- | The AWS account to which the transfer was made.
--
-- /Note:/ Consider using 'transferredTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTransferredTo :: Lens.Lens' OutgoingCertificate (Core.Maybe Types.AwsAccountId)
ocTransferredTo = Lens.field @"transferredTo"
{-# INLINEABLE ocTransferredTo #-}
{-# DEPRECATED transferredTo "Use generic-lens or generic-optics with 'transferredTo' instead"  #-}

instance Core.FromJSON OutgoingCertificate where
        parseJSON
          = Core.withObject "OutgoingCertificate" Core.$
              \ x ->
                OutgoingCertificate' Core.<$>
                  (x Core..:? "certificateArn") Core.<*> x Core..:? "certificateId"
                    Core.<*> x Core..:? "creationDate"
                    Core.<*> x Core..:? "transferDate"
                    Core.<*> x Core..:? "transferMessage"
                    Core.<*> x Core..:? "transferredTo"
