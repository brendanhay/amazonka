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
-- Module      : Amazonka.GamesParks.GetGameConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration of the game.
module Amazonka.GamesParks.GetGameConfiguration
  ( -- * Creating a Request
    GetGameConfiguration (..),
    newGetGameConfiguration,

    -- * Request Lenses
    getGameConfiguration_sections,
    getGameConfiguration_gameName,

    -- * Destructuring the Response
    GetGameConfigurationResponse (..),
    newGetGameConfigurationResponse,

    -- * Response Lenses
    getGameConfigurationResponse_gameConfiguration,
    getGameConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGameConfiguration' smart constructor.
data GetGameConfiguration = GetGameConfiguration'
  { -- | The list of sections to return.
    sections :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the game.
    gameName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGameConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sections', 'getGameConfiguration_sections' - The list of sections to return.
--
-- 'gameName', 'getGameConfiguration_gameName' - The name of the game.
newGetGameConfiguration ::
  -- | 'gameName'
  Prelude.Text ->
  GetGameConfiguration
newGetGameConfiguration pGameName_ =
  GetGameConfiguration'
    { sections = Prelude.Nothing,
      gameName = pGameName_
    }

-- | The list of sections to return.
getGameConfiguration_sections :: Lens.Lens' GetGameConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getGameConfiguration_sections = Lens.lens (\GetGameConfiguration' {sections} -> sections) (\s@GetGameConfiguration' {} a -> s {sections = a} :: GetGameConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the game.
getGameConfiguration_gameName :: Lens.Lens' GetGameConfiguration Prelude.Text
getGameConfiguration_gameName = Lens.lens (\GetGameConfiguration' {gameName} -> gameName) (\s@GetGameConfiguration' {} a -> s {gameName = a} :: GetGameConfiguration)

instance Core.AWSRequest GetGameConfiguration where
  type
    AWSResponse GetGameConfiguration =
      GetGameConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGameConfigurationResponse'
            Prelude.<$> (x Data..?> "GameConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGameConfiguration where
  hashWithSalt _salt GetGameConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` sections
      `Prelude.hashWithSalt` gameName

instance Prelude.NFData GetGameConfiguration where
  rnf GetGameConfiguration' {..} =
    Prelude.rnf sections
      `Prelude.seq` Prelude.rnf gameName

instance Data.ToHeaders GetGameConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetGameConfiguration where
  toPath GetGameConfiguration' {..} =
    Prelude.mconcat
      ["/game/", Data.toBS gameName, "/configuration"]

instance Data.ToQuery GetGameConfiguration where
  toQuery GetGameConfiguration' {..} =
    Prelude.mconcat
      [ "Sections"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> sections)
      ]

-- | /See:/ 'newGetGameConfigurationResponse' smart constructor.
data GetGameConfigurationResponse = GetGameConfigurationResponse'
  { -- | Details about the game configuration.
    gameConfiguration :: Prelude.Maybe GameConfigurationDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGameConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameConfiguration', 'getGameConfigurationResponse_gameConfiguration' - Details about the game configuration.
--
-- 'httpStatus', 'getGameConfigurationResponse_httpStatus' - The response's http status code.
newGetGameConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGameConfigurationResponse
newGetGameConfigurationResponse pHttpStatus_ =
  GetGameConfigurationResponse'
    { gameConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the game configuration.
getGameConfigurationResponse_gameConfiguration :: Lens.Lens' GetGameConfigurationResponse (Prelude.Maybe GameConfigurationDetails)
getGameConfigurationResponse_gameConfiguration = Lens.lens (\GetGameConfigurationResponse' {gameConfiguration} -> gameConfiguration) (\s@GetGameConfigurationResponse' {} a -> s {gameConfiguration = a} :: GetGameConfigurationResponse)

-- | The response's http status code.
getGameConfigurationResponse_httpStatus :: Lens.Lens' GetGameConfigurationResponse Prelude.Int
getGameConfigurationResponse_httpStatus = Lens.lens (\GetGameConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetGameConfigurationResponse' {} a -> s {httpStatus = a} :: GetGameConfigurationResponse)

instance Prelude.NFData GetGameConfigurationResponse where
  rnf GetGameConfigurationResponse' {..} =
    Prelude.rnf gameConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
