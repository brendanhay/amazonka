{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.MessageDsn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.MessageDsn
  ( MessageDsn (..)
  -- * Smart constructor
  , mkMessageDsn
  -- * Lenses
  , mdReportingMta
  , mdArrivalDate
  , mdExtensionFields
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.ExtensionField as Types
import qualified Network.AWS.SES.Types.ReportingMta as Types

-- | Message-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkMessageDsn' smart constructor.
data MessageDsn = MessageDsn'
  { reportingMta :: Types.ReportingMta
    -- ^ The reporting MTA that attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). The default value is @dns; inbound-smtp.[region].amazonaws.com@ .
  , arrivalDate :: Core.Maybe Core.UTCTime
    -- ^ When the message was received by the reporting mail transfer agent (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
  , extensionFields :: Core.Maybe [Types.ExtensionField]
    -- ^ Additional X-headers to include in the DSN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MessageDsn' value with any optional fields omitted.
mkMessageDsn
    :: Types.ReportingMta -- ^ 'reportingMta'
    -> MessageDsn
mkMessageDsn reportingMta
  = MessageDsn'{reportingMta, arrivalDate = Core.Nothing,
                extensionFields = Core.Nothing}

-- | The reporting MTA that attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). The default value is @dns; inbound-smtp.[region].amazonaws.com@ .
--
-- /Note:/ Consider using 'reportingMta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdReportingMta :: Lens.Lens' MessageDsn Types.ReportingMta
mdReportingMta = Lens.field @"reportingMta"
{-# INLINEABLE mdReportingMta #-}
{-# DEPRECATED reportingMta "Use generic-lens or generic-optics with 'reportingMta' instead"  #-}

-- | When the message was received by the reporting mail transfer agent (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
--
-- /Note:/ Consider using 'arrivalDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdArrivalDate :: Lens.Lens' MessageDsn (Core.Maybe Core.UTCTime)
mdArrivalDate = Lens.field @"arrivalDate"
{-# INLINEABLE mdArrivalDate #-}
{-# DEPRECATED arrivalDate "Use generic-lens or generic-optics with 'arrivalDate' instead"  #-}

-- | Additional X-headers to include in the DSN.
--
-- /Note:/ Consider using 'extensionFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdExtensionFields :: Lens.Lens' MessageDsn (Core.Maybe [Types.ExtensionField])
mdExtensionFields = Lens.field @"extensionFields"
{-# INLINEABLE mdExtensionFields #-}
{-# DEPRECATED extensionFields "Use generic-lens or generic-optics with 'extensionFields' instead"  #-}

instance Core.ToQuery MessageDsn where
        toQuery MessageDsn{..}
          = Core.toQueryPair "ReportingMta" reportingMta Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ArrivalDate") arrivalDate
              Core.<>
              Core.toQueryPair "ExtensionFields"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   extensionFields)
