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
-- Module      : Amazonka.Lightsail.Types.NameServersUpdateState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.NameServersUpdateState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.NameServersUpdateStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of the name server records update made by Amazon
-- Lightsail to an Amazon Route 53 registered domain.
--
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/understanding-dns-in-amazon-lightsail DNS in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newNameServersUpdateState' smart constructor.
data NameServersUpdateState = NameServersUpdateState'
  { -- | The status code for the name servers update.
    --
    -- Following are the possible values:
    --
    -- -   @SUCCEEDED@ - The name server records were successfully updated.
    --
    -- -   @PENDING@ - The name server record update is in progress.
    --
    -- -   @FAILED@ - The name server record update failed.
    --
    -- -   @STARTED@ - The automatic name server record update started.
    code :: Prelude.Maybe NameServersUpdateStateCode,
    -- | The message that describes the reason for the status code.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NameServersUpdateState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'nameServersUpdateState_code' - The status code for the name servers update.
--
-- Following are the possible values:
--
-- -   @SUCCEEDED@ - The name server records were successfully updated.
--
-- -   @PENDING@ - The name server record update is in progress.
--
-- -   @FAILED@ - The name server record update failed.
--
-- -   @STARTED@ - The automatic name server record update started.
--
-- 'message', 'nameServersUpdateState_message' - The message that describes the reason for the status code.
newNameServersUpdateState ::
  NameServersUpdateState
newNameServersUpdateState =
  NameServersUpdateState'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status code for the name servers update.
--
-- Following are the possible values:
--
-- -   @SUCCEEDED@ - The name server records were successfully updated.
--
-- -   @PENDING@ - The name server record update is in progress.
--
-- -   @FAILED@ - The name server record update failed.
--
-- -   @STARTED@ - The automatic name server record update started.
nameServersUpdateState_code :: Lens.Lens' NameServersUpdateState (Prelude.Maybe NameServersUpdateStateCode)
nameServersUpdateState_code = Lens.lens (\NameServersUpdateState' {code} -> code) (\s@NameServersUpdateState' {} a -> s {code = a} :: NameServersUpdateState)

-- | The message that describes the reason for the status code.
nameServersUpdateState_message :: Lens.Lens' NameServersUpdateState (Prelude.Maybe Prelude.Text)
nameServersUpdateState_message = Lens.lens (\NameServersUpdateState' {message} -> message) (\s@NameServersUpdateState' {} a -> s {message = a} :: NameServersUpdateState)

instance Data.FromJSON NameServersUpdateState where
  parseJSON =
    Data.withObject
      "NameServersUpdateState"
      ( \x ->
          NameServersUpdateState'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable NameServersUpdateState where
  hashWithSalt _salt NameServersUpdateState' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData NameServersUpdateState where
  rnf NameServersUpdateState' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
