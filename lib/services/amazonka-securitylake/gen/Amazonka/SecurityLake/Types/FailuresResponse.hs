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
-- Module      : Amazonka.SecurityLake.Types.FailuresResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.FailuresResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.Failures

-- | Response element for actions which make changes namely create, update,
-- or delete actions.
--
-- /See:/ 'newFailuresResponse' smart constructor.
data FailuresResponse = FailuresResponse'
  { -- | List of all failures.
    failures :: Prelude.Maybe [Failures],
    -- | List of Regions where the failure occurred.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailuresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'failuresResponse_failures' - List of all failures.
--
-- 'region', 'failuresResponse_region' - List of Regions where the failure occurred.
newFailuresResponse ::
  FailuresResponse
newFailuresResponse =
  FailuresResponse'
    { failures = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | List of all failures.
failuresResponse_failures :: Lens.Lens' FailuresResponse (Prelude.Maybe [Failures])
failuresResponse_failures = Lens.lens (\FailuresResponse' {failures} -> failures) (\s@FailuresResponse' {} a -> s {failures = a} :: FailuresResponse) Prelude.. Lens.mapping Lens.coerced

-- | List of Regions where the failure occurred.
failuresResponse_region :: Lens.Lens' FailuresResponse (Prelude.Maybe Prelude.Text)
failuresResponse_region = Lens.lens (\FailuresResponse' {region} -> region) (\s@FailuresResponse' {} a -> s {region = a} :: FailuresResponse)

instance Data.FromJSON FailuresResponse where
  parseJSON =
    Data.withObject
      "FailuresResponse"
      ( \x ->
          FailuresResponse'
            Prelude.<$> (x Data..:? "failures" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "region")
      )

instance Prelude.Hashable FailuresResponse where
  hashWithSalt _salt FailuresResponse' {..} =
    _salt `Prelude.hashWithSalt` failures
      `Prelude.hashWithSalt` region

instance Prelude.NFData FailuresResponse where
  rnf FailuresResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf region
