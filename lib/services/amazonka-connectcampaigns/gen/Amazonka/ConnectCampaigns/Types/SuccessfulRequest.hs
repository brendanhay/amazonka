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
-- Module      : Amazonka.ConnectCampaigns.Types.SuccessfulRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.SuccessfulRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A successful request identified by the unique client token.
--
-- /See:/ 'newSuccessfulRequest' smart constructor.
data SuccessfulRequest = SuccessfulRequest'
  { clientToken :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuccessfulRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'successfulRequest_clientToken' - Undocumented member.
--
-- 'id', 'successfulRequest_id' - Undocumented member.
newSuccessfulRequest ::
  SuccessfulRequest
newSuccessfulRequest =
  SuccessfulRequest'
    { clientToken = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Undocumented member.
successfulRequest_clientToken :: Lens.Lens' SuccessfulRequest (Prelude.Maybe Prelude.Text)
successfulRequest_clientToken = Lens.lens (\SuccessfulRequest' {clientToken} -> clientToken) (\s@SuccessfulRequest' {} a -> s {clientToken = a} :: SuccessfulRequest)

-- | Undocumented member.
successfulRequest_id :: Lens.Lens' SuccessfulRequest (Prelude.Maybe Prelude.Text)
successfulRequest_id = Lens.lens (\SuccessfulRequest' {id} -> id) (\s@SuccessfulRequest' {} a -> s {id = a} :: SuccessfulRequest)

instance Data.FromJSON SuccessfulRequest where
  parseJSON =
    Data.withObject
      "SuccessfulRequest"
      ( \x ->
          SuccessfulRequest'
            Prelude.<$> (x Data..:? "clientToken")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable SuccessfulRequest where
  hashWithSalt _salt SuccessfulRequest' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` id

instance Prelude.NFData SuccessfulRequest where
  rnf SuccessfulRequest' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf id
