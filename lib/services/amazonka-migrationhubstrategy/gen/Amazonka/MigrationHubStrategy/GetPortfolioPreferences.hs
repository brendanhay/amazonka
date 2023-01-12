{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubStrategy.GetPortfolioPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your migration and modernization preferences.
module Amazonka.MigrationHubStrategy.GetPortfolioPreferences
  ( -- * Creating a Request
    GetPortfolioPreferences (..),
    newGetPortfolioPreferences,

    -- * Destructuring the Response
    GetPortfolioPreferencesResponse (..),
    newGetPortfolioPreferencesResponse,

    -- * Response Lenses
    getPortfolioPreferencesResponse_applicationMode,
    getPortfolioPreferencesResponse_applicationPreferences,
    getPortfolioPreferencesResponse_databasePreferences,
    getPortfolioPreferencesResponse_prioritizeBusinessGoals,
    getPortfolioPreferencesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPortfolioPreferences' smart constructor.
data GetPortfolioPreferences = GetPortfolioPreferences'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPortfolioPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetPortfolioPreferences ::
  GetPortfolioPreferences
newGetPortfolioPreferences = GetPortfolioPreferences'

instance Core.AWSRequest GetPortfolioPreferences where
  type
    AWSResponse GetPortfolioPreferences =
      GetPortfolioPreferencesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPortfolioPreferencesResponse'
            Prelude.<$> (x Data..?> "applicationMode")
            Prelude.<*> (x Data..?> "applicationPreferences")
            Prelude.<*> (x Data..?> "databasePreferences")
            Prelude.<*> (x Data..?> "prioritizeBusinessGoals")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPortfolioPreferences where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetPortfolioPreferences where
  rnf _ = ()

instance Data.ToHeaders GetPortfolioPreferences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPortfolioPreferences where
  toPath = Prelude.const "/get-portfolio-preferences"

instance Data.ToQuery GetPortfolioPreferences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPortfolioPreferencesResponse' smart constructor.
data GetPortfolioPreferencesResponse = GetPortfolioPreferencesResponse'
  { -- | The classification for application component types.
    applicationMode :: Prelude.Maybe ApplicationMode,
    -- | The transformation preferences for non-database applications.
    applicationPreferences :: Prelude.Maybe ApplicationPreferences,
    -- | The transformation preferences for database applications.
    databasePreferences :: Prelude.Maybe DatabasePreferences,
    -- | The rank of business goals based on priority.
    prioritizeBusinessGoals :: Prelude.Maybe PrioritizeBusinessGoals,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPortfolioPreferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationMode', 'getPortfolioPreferencesResponse_applicationMode' - The classification for application component types.
--
-- 'applicationPreferences', 'getPortfolioPreferencesResponse_applicationPreferences' - The transformation preferences for non-database applications.
--
-- 'databasePreferences', 'getPortfolioPreferencesResponse_databasePreferences' - The transformation preferences for database applications.
--
-- 'prioritizeBusinessGoals', 'getPortfolioPreferencesResponse_prioritizeBusinessGoals' - The rank of business goals based on priority.
--
-- 'httpStatus', 'getPortfolioPreferencesResponse_httpStatus' - The response's http status code.
newGetPortfolioPreferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPortfolioPreferencesResponse
newGetPortfolioPreferencesResponse pHttpStatus_ =
  GetPortfolioPreferencesResponse'
    { applicationMode =
        Prelude.Nothing,
      applicationPreferences = Prelude.Nothing,
      databasePreferences = Prelude.Nothing,
      prioritizeBusinessGoals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The classification for application component types.
getPortfolioPreferencesResponse_applicationMode :: Lens.Lens' GetPortfolioPreferencesResponse (Prelude.Maybe ApplicationMode)
getPortfolioPreferencesResponse_applicationMode = Lens.lens (\GetPortfolioPreferencesResponse' {applicationMode} -> applicationMode) (\s@GetPortfolioPreferencesResponse' {} a -> s {applicationMode = a} :: GetPortfolioPreferencesResponse)

-- | The transformation preferences for non-database applications.
getPortfolioPreferencesResponse_applicationPreferences :: Lens.Lens' GetPortfolioPreferencesResponse (Prelude.Maybe ApplicationPreferences)
getPortfolioPreferencesResponse_applicationPreferences = Lens.lens (\GetPortfolioPreferencesResponse' {applicationPreferences} -> applicationPreferences) (\s@GetPortfolioPreferencesResponse' {} a -> s {applicationPreferences = a} :: GetPortfolioPreferencesResponse)

-- | The transformation preferences for database applications.
getPortfolioPreferencesResponse_databasePreferences :: Lens.Lens' GetPortfolioPreferencesResponse (Prelude.Maybe DatabasePreferences)
getPortfolioPreferencesResponse_databasePreferences = Lens.lens (\GetPortfolioPreferencesResponse' {databasePreferences} -> databasePreferences) (\s@GetPortfolioPreferencesResponse' {} a -> s {databasePreferences = a} :: GetPortfolioPreferencesResponse)

-- | The rank of business goals based on priority.
getPortfolioPreferencesResponse_prioritizeBusinessGoals :: Lens.Lens' GetPortfolioPreferencesResponse (Prelude.Maybe PrioritizeBusinessGoals)
getPortfolioPreferencesResponse_prioritizeBusinessGoals = Lens.lens (\GetPortfolioPreferencesResponse' {prioritizeBusinessGoals} -> prioritizeBusinessGoals) (\s@GetPortfolioPreferencesResponse' {} a -> s {prioritizeBusinessGoals = a} :: GetPortfolioPreferencesResponse)

-- | The response's http status code.
getPortfolioPreferencesResponse_httpStatus :: Lens.Lens' GetPortfolioPreferencesResponse Prelude.Int
getPortfolioPreferencesResponse_httpStatus = Lens.lens (\GetPortfolioPreferencesResponse' {httpStatus} -> httpStatus) (\s@GetPortfolioPreferencesResponse' {} a -> s {httpStatus = a} :: GetPortfolioPreferencesResponse)

instance
  Prelude.NFData
    GetPortfolioPreferencesResponse
  where
  rnf GetPortfolioPreferencesResponse' {..} =
    Prelude.rnf applicationMode
      `Prelude.seq` Prelude.rnf applicationPreferences
      `Prelude.seq` Prelude.rnf databasePreferences
      `Prelude.seq` Prelude.rnf prioritizeBusinessGoals
      `Prelude.seq` Prelude.rnf httpStatus
