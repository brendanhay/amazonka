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
-- Module      : Amazonka.Chime.Types.SipMediaApplicationCall
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.SipMediaApplicationCall where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A @Call@ instance for a SIP media application.
--
-- /See:/ 'newSipMediaApplicationCall' smart constructor.
data SipMediaApplicationCall = SipMediaApplicationCall'
  { -- | The transaction ID of a call.
    transactionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SipMediaApplicationCall' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'sipMediaApplicationCall_transactionId' - The transaction ID of a call.
newSipMediaApplicationCall ::
  SipMediaApplicationCall
newSipMediaApplicationCall =
  SipMediaApplicationCall'
    { transactionId =
        Prelude.Nothing
    }

-- | The transaction ID of a call.
sipMediaApplicationCall_transactionId :: Lens.Lens' SipMediaApplicationCall (Prelude.Maybe Prelude.Text)
sipMediaApplicationCall_transactionId = Lens.lens (\SipMediaApplicationCall' {transactionId} -> transactionId) (\s@SipMediaApplicationCall' {} a -> s {transactionId = a} :: SipMediaApplicationCall)

instance Data.FromJSON SipMediaApplicationCall where
  parseJSON =
    Data.withObject
      "SipMediaApplicationCall"
      ( \x ->
          SipMediaApplicationCall'
            Prelude.<$> (x Data..:? "TransactionId")
      )

instance Prelude.Hashable SipMediaApplicationCall where
  hashWithSalt _salt SipMediaApplicationCall' {..} =
    _salt `Prelude.hashWithSalt` transactionId

instance Prelude.NFData SipMediaApplicationCall where
  rnf SipMediaApplicationCall' {..} =
    Prelude.rnf transactionId
