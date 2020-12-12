{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.MessageDsn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.MessageDsn
  ( MessageDsn (..),

    -- * Smart constructor
    mkMessageDsn,

    -- * Lenses
    mdArrivalDate,
    mdExtensionFields,
    mdReportingMta,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.ExtensionField

-- | Message-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkMessageDsn' smart constructor.
data MessageDsn = MessageDsn'
  { arrivalDate ::
      Lude.Maybe Lude.DateTime,
    extensionFields :: Lude.Maybe [ExtensionField],
    reportingMta :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageDsn' with the minimum fields required to make a request.
--
-- * 'arrivalDate' - When the message was received by the reporting mail transfer agent (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
-- * 'extensionFields' - Additional X-headers to include in the DSN.
-- * 'reportingMta' - The reporting MTA that attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). The default value is @dns; inbound-smtp.[region].amazonaws.com@ .
mkMessageDsn ::
  -- | 'reportingMta'
  Lude.Text ->
  MessageDsn
mkMessageDsn pReportingMta_ =
  MessageDsn'
    { arrivalDate = Lude.Nothing,
      extensionFields = Lude.Nothing,
      reportingMta = pReportingMta_
    }

-- | When the message was received by the reporting mail transfer agent (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
--
-- /Note:/ Consider using 'arrivalDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdArrivalDate :: Lens.Lens' MessageDsn (Lude.Maybe Lude.DateTime)
mdArrivalDate = Lens.lens (arrivalDate :: MessageDsn -> Lude.Maybe Lude.DateTime) (\s a -> s {arrivalDate = a} :: MessageDsn)
{-# DEPRECATED mdArrivalDate "Use generic-lens or generic-optics with 'arrivalDate' instead." #-}

-- | Additional X-headers to include in the DSN.
--
-- /Note:/ Consider using 'extensionFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdExtensionFields :: Lens.Lens' MessageDsn (Lude.Maybe [ExtensionField])
mdExtensionFields = Lens.lens (extensionFields :: MessageDsn -> Lude.Maybe [ExtensionField]) (\s a -> s {extensionFields = a} :: MessageDsn)
{-# DEPRECATED mdExtensionFields "Use generic-lens or generic-optics with 'extensionFields' instead." #-}

-- | The reporting MTA that attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). The default value is @dns; inbound-smtp.[region].amazonaws.com@ .
--
-- /Note:/ Consider using 'reportingMta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdReportingMta :: Lens.Lens' MessageDsn Lude.Text
mdReportingMta = Lens.lens (reportingMta :: MessageDsn -> Lude.Text) (\s a -> s {reportingMta = a} :: MessageDsn)
{-# DEPRECATED mdReportingMta "Use generic-lens or generic-optics with 'reportingMta' instead." #-}

instance Lude.ToQuery MessageDsn where
  toQuery MessageDsn' {..} =
    Lude.mconcat
      [ "ArrivalDate" Lude.=: arrivalDate,
        "ExtensionFields"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> extensionFields),
        "ReportingMta" Lude.=: reportingMta
      ]
