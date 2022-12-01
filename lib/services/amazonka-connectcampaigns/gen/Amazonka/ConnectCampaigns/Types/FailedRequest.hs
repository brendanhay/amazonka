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
-- Module      : Amazonka.ConnectCampaigns.Types.FailedRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.FailedRequest where

import Amazonka.ConnectCampaigns.Types.FailureCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A failed request identified by the unique client token.
--
-- /See:/ 'newFailedRequest' smart constructor.
data FailedRequest = FailedRequest'
  { clientToken :: Prelude.Maybe Prelude.Text,
    failureCode :: Prelude.Maybe FailureCode,
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'failedRequest_clientToken' - Undocumented member.
--
-- 'failureCode', 'failedRequest_failureCode' - Undocumented member.
--
-- 'id', 'failedRequest_id' - Undocumented member.
newFailedRequest ::
  FailedRequest
newFailedRequest =
  FailedRequest'
    { clientToken = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Undocumented member.
failedRequest_clientToken :: Lens.Lens' FailedRequest (Prelude.Maybe Prelude.Text)
failedRequest_clientToken = Lens.lens (\FailedRequest' {clientToken} -> clientToken) (\s@FailedRequest' {} a -> s {clientToken = a} :: FailedRequest)

-- | Undocumented member.
failedRequest_failureCode :: Lens.Lens' FailedRequest (Prelude.Maybe FailureCode)
failedRequest_failureCode = Lens.lens (\FailedRequest' {failureCode} -> failureCode) (\s@FailedRequest' {} a -> s {failureCode = a} :: FailedRequest)

-- | Undocumented member.
failedRequest_id :: Lens.Lens' FailedRequest (Prelude.Maybe Prelude.Text)
failedRequest_id = Lens.lens (\FailedRequest' {id} -> id) (\s@FailedRequest' {} a -> s {id = a} :: FailedRequest)

instance Core.FromJSON FailedRequest where
  parseJSON =
    Core.withObject
      "FailedRequest"
      ( \x ->
          FailedRequest'
            Prelude.<$> (x Core..:? "clientToken")
            Prelude.<*> (x Core..:? "failureCode")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable FailedRequest where
  hashWithSalt _salt FailedRequest' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` id

instance Prelude.NFData FailedRequest where
  rnf FailedRequest' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf id
