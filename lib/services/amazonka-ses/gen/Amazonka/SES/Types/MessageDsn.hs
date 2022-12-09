{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SES.Types.MessageDsn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.MessageDsn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.ExtensionField

-- | Message-related information to include in the Delivery Status
-- Notification (DSN) when an email that Amazon SES receives on your behalf
-- bounces.
--
-- For information about receiving email through Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide>.
--
-- /See:/ 'newMessageDsn' smart constructor.
data MessageDsn = MessageDsn'
  { -- | When the message was received by the reporting mail transfer agent
    -- (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time
    -- format.
    arrivalDate :: Prelude.Maybe Data.ISO8601,
    -- | Additional X-headers to include in the DSN.
    extensionFields :: Prelude.Maybe [ExtensionField],
    -- | The reporting MTA that attempted to deliver the message, formatted as
    -- specified in <https://tools.ietf.org/html/rfc3464 RFC 3464>
    -- (@mta-name-type; mta-name@). The default value is
    -- @dns; inbound-smtp.[region].amazonaws.com@.
    reportingMta :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageDsn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrivalDate', 'messageDsn_arrivalDate' - When the message was received by the reporting mail transfer agent
-- (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time
-- format.
--
-- 'extensionFields', 'messageDsn_extensionFields' - Additional X-headers to include in the DSN.
--
-- 'reportingMta', 'messageDsn_reportingMta' - The reporting MTA that attempted to deliver the message, formatted as
-- specified in <https://tools.ietf.org/html/rfc3464 RFC 3464>
-- (@mta-name-type; mta-name@). The default value is
-- @dns; inbound-smtp.[region].amazonaws.com@.
newMessageDsn ::
  -- | 'reportingMta'
  Prelude.Text ->
  MessageDsn
newMessageDsn pReportingMta_ =
  MessageDsn'
    { arrivalDate = Prelude.Nothing,
      extensionFields = Prelude.Nothing,
      reportingMta = pReportingMta_
    }

-- | When the message was received by the reporting mail transfer agent
-- (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time
-- format.
messageDsn_arrivalDate :: Lens.Lens' MessageDsn (Prelude.Maybe Prelude.UTCTime)
messageDsn_arrivalDate = Lens.lens (\MessageDsn' {arrivalDate} -> arrivalDate) (\s@MessageDsn' {} a -> s {arrivalDate = a} :: MessageDsn) Prelude.. Lens.mapping Data._Time

-- | Additional X-headers to include in the DSN.
messageDsn_extensionFields :: Lens.Lens' MessageDsn (Prelude.Maybe [ExtensionField])
messageDsn_extensionFields = Lens.lens (\MessageDsn' {extensionFields} -> extensionFields) (\s@MessageDsn' {} a -> s {extensionFields = a} :: MessageDsn) Prelude.. Lens.mapping Lens.coerced

-- | The reporting MTA that attempted to deliver the message, formatted as
-- specified in <https://tools.ietf.org/html/rfc3464 RFC 3464>
-- (@mta-name-type; mta-name@). The default value is
-- @dns; inbound-smtp.[region].amazonaws.com@.
messageDsn_reportingMta :: Lens.Lens' MessageDsn Prelude.Text
messageDsn_reportingMta = Lens.lens (\MessageDsn' {reportingMta} -> reportingMta) (\s@MessageDsn' {} a -> s {reportingMta = a} :: MessageDsn)

instance Prelude.Hashable MessageDsn where
  hashWithSalt _salt MessageDsn' {..} =
    _salt `Prelude.hashWithSalt` arrivalDate
      `Prelude.hashWithSalt` extensionFields
      `Prelude.hashWithSalt` reportingMta

instance Prelude.NFData MessageDsn where
  rnf MessageDsn' {..} =
    Prelude.rnf arrivalDate
      `Prelude.seq` Prelude.rnf extensionFields
      `Prelude.seq` Prelude.rnf reportingMta

instance Data.ToQuery MessageDsn where
  toQuery MessageDsn' {..} =
    Prelude.mconcat
      [ "ArrivalDate" Data.=: arrivalDate,
        "ExtensionFields"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> extensionFields
            ),
        "ReportingMta" Data.=: reportingMta
      ]
