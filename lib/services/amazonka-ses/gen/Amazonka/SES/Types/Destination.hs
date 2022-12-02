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
-- Module      : Amazonka.SES.Types.Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the destination of the message, consisting of To:, CC:, and
-- BCC: fields.
--
-- Amazon SES does not support the SMTPUTF8 extension, as described in
-- <https://tools.ietf.org/html/rfc6531 RFC6531>. For this reason, the
-- /local part/ of a destination email address (the part of the email
-- address that precedes the \@ sign) may only contain
-- <https://en.wikipedia.org/wiki/Email_address#Local-part 7-bit ASCII characters>.
-- If the /domain part/ of an address (the part after the \@ sign) contains
-- non-ASCII characters, they must be encoded using Punycode, as described
-- in <https://tools.ietf.org/html/rfc3492.html RFC3492>.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | The recipients to place on the CC: line of the message.
    ccAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The recipients to place on the BCC: line of the message.
    bccAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The recipients to place on the To: line of the message.
    toAddresses :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ccAddresses', 'destination_ccAddresses' - The recipients to place on the CC: line of the message.
--
-- 'bccAddresses', 'destination_bccAddresses' - The recipients to place on the BCC: line of the message.
--
-- 'toAddresses', 'destination_toAddresses' - The recipients to place on the To: line of the message.
newDestination ::
  Destination
newDestination =
  Destination'
    { ccAddresses = Prelude.Nothing,
      bccAddresses = Prelude.Nothing,
      toAddresses = Prelude.Nothing
    }

-- | The recipients to place on the CC: line of the message.
destination_ccAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_ccAddresses = Lens.lens (\Destination' {ccAddresses} -> ccAddresses) (\s@Destination' {} a -> s {ccAddresses = a} :: Destination) Prelude.. Lens.mapping Lens.coerced

-- | The recipients to place on the BCC: line of the message.
destination_bccAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_bccAddresses = Lens.lens (\Destination' {bccAddresses} -> bccAddresses) (\s@Destination' {} a -> s {bccAddresses = a} :: Destination) Prelude.. Lens.mapping Lens.coerced

-- | The recipients to place on the To: line of the message.
destination_toAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_toAddresses = Lens.lens (\Destination' {toAddresses} -> toAddresses) (\s@Destination' {} a -> s {toAddresses = a} :: Destination) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt `Prelude.hashWithSalt` ccAddresses
      `Prelude.hashWithSalt` bccAddresses
      `Prelude.hashWithSalt` toAddresses

instance Prelude.NFData Destination where
  rnf Destination' {..} =
    Prelude.rnf ccAddresses
      `Prelude.seq` Prelude.rnf bccAddresses
      `Prelude.seq` Prelude.rnf toAddresses

instance Data.ToQuery Destination where
  toQuery Destination' {..} =
    Prelude.mconcat
      [ "CcAddresses"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> ccAddresses),
        "BccAddresses"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> bccAddresses),
        "ToAddresses"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> toAddresses)
      ]
