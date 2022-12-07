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
-- Module      : Amazonka.Lightsail.Types.R53HostedZoneDeletionState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.R53HostedZoneDeletionState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.R53HostedZoneDeletionStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the deletion state of an Amazon Route 53 hosted zone for a
-- domain that is being automatically delegated to an Amazon Lightsail DNS
-- zone.
--
-- /See:/ 'newR53HostedZoneDeletionState' smart constructor.
data R53HostedZoneDeletionState = R53HostedZoneDeletionState'
  { -- | The message that describes the reason for the status code.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code for the deletion state.
    --
    -- Following are the possible values:
    --
    -- -   @SUCCEEDED@ - The hosted zone was successfully deleted.
    --
    -- -   @PENDING@ - The hosted zone deletion is in progress.
    --
    -- -   @FAILED@ - The hosted zone deletion failed.
    --
    -- -   @STARTED@ - The hosted zone deletion started.
    code :: Prelude.Maybe R53HostedZoneDeletionStateCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'R53HostedZoneDeletionState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'r53HostedZoneDeletionState_message' - The message that describes the reason for the status code.
--
-- 'code', 'r53HostedZoneDeletionState_code' - The status code for the deletion state.
--
-- Following are the possible values:
--
-- -   @SUCCEEDED@ - The hosted zone was successfully deleted.
--
-- -   @PENDING@ - The hosted zone deletion is in progress.
--
-- -   @FAILED@ - The hosted zone deletion failed.
--
-- -   @STARTED@ - The hosted zone deletion started.
newR53HostedZoneDeletionState ::
  R53HostedZoneDeletionState
newR53HostedZoneDeletionState =
  R53HostedZoneDeletionState'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The message that describes the reason for the status code.
r53HostedZoneDeletionState_message :: Lens.Lens' R53HostedZoneDeletionState (Prelude.Maybe Prelude.Text)
r53HostedZoneDeletionState_message = Lens.lens (\R53HostedZoneDeletionState' {message} -> message) (\s@R53HostedZoneDeletionState' {} a -> s {message = a} :: R53HostedZoneDeletionState)

-- | The status code for the deletion state.
--
-- Following are the possible values:
--
-- -   @SUCCEEDED@ - The hosted zone was successfully deleted.
--
-- -   @PENDING@ - The hosted zone deletion is in progress.
--
-- -   @FAILED@ - The hosted zone deletion failed.
--
-- -   @STARTED@ - The hosted zone deletion started.
r53HostedZoneDeletionState_code :: Lens.Lens' R53HostedZoneDeletionState (Prelude.Maybe R53HostedZoneDeletionStateCode)
r53HostedZoneDeletionState_code = Lens.lens (\R53HostedZoneDeletionState' {code} -> code) (\s@R53HostedZoneDeletionState' {} a -> s {code = a} :: R53HostedZoneDeletionState)

instance Data.FromJSON R53HostedZoneDeletionState where
  parseJSON =
    Data.withObject
      "R53HostedZoneDeletionState"
      ( \x ->
          R53HostedZoneDeletionState'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "code")
      )

instance Prelude.Hashable R53HostedZoneDeletionState where
  hashWithSalt _salt R53HostedZoneDeletionState' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData R53HostedZoneDeletionState where
  rnf R53HostedZoneDeletionState' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
