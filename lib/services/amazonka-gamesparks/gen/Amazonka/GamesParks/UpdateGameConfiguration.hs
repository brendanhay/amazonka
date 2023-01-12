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
-- Module      : Amazonka.GamesParks.UpdateGameConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more sections of the game configuration.
module Amazonka.GamesParks.UpdateGameConfiguration
  ( -- * Creating a Request
    UpdateGameConfiguration (..),
    newUpdateGameConfiguration,

    -- * Request Lenses
    updateGameConfiguration_gameName,
    updateGameConfiguration_modifications,

    -- * Destructuring the Response
    UpdateGameConfigurationResponse (..),
    newUpdateGameConfigurationResponse,

    -- * Response Lenses
    updateGameConfigurationResponse_gameConfiguration,
    updateGameConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGameConfiguration' smart constructor.
data UpdateGameConfiguration = UpdateGameConfiguration'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The list of modifications to make.
    modifications :: Prelude.NonEmpty SectionModification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'updateGameConfiguration_gameName' - The name of the game.
--
-- 'modifications', 'updateGameConfiguration_modifications' - The list of modifications to make.
newUpdateGameConfiguration ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'modifications'
  Prelude.NonEmpty SectionModification ->
  UpdateGameConfiguration
newUpdateGameConfiguration pGameName_ pModifications_ =
  UpdateGameConfiguration'
    { gameName = pGameName_,
      modifications =
        Lens.coerced Lens.# pModifications_
    }

-- | The name of the game.
updateGameConfiguration_gameName :: Lens.Lens' UpdateGameConfiguration Prelude.Text
updateGameConfiguration_gameName = Lens.lens (\UpdateGameConfiguration' {gameName} -> gameName) (\s@UpdateGameConfiguration' {} a -> s {gameName = a} :: UpdateGameConfiguration)

-- | The list of modifications to make.
updateGameConfiguration_modifications :: Lens.Lens' UpdateGameConfiguration (Prelude.NonEmpty SectionModification)
updateGameConfiguration_modifications = Lens.lens (\UpdateGameConfiguration' {modifications} -> modifications) (\s@UpdateGameConfiguration' {} a -> s {modifications = a} :: UpdateGameConfiguration) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateGameConfiguration where
  type
    AWSResponse UpdateGameConfiguration =
      UpdateGameConfigurationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameConfigurationResponse'
            Prelude.<$> (x Data..?> "GameConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGameConfiguration where
  hashWithSalt _salt UpdateGameConfiguration' {..} =
    _salt `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` modifications

instance Prelude.NFData UpdateGameConfiguration where
  rnf UpdateGameConfiguration' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf modifications

instance Data.ToHeaders UpdateGameConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGameConfiguration where
  toJSON UpdateGameConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("Modifications" Data..= modifications)
          ]
      )

instance Data.ToPath UpdateGameConfiguration where
  toPath UpdateGameConfiguration' {..} =
    Prelude.mconcat
      ["/game/", Data.toBS gameName, "/configuration"]

instance Data.ToQuery UpdateGameConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGameConfigurationResponse' smart constructor.
data UpdateGameConfigurationResponse = UpdateGameConfigurationResponse'
  { -- | Details about the game configuration.
    gameConfiguration :: Prelude.Maybe GameConfigurationDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameConfiguration', 'updateGameConfigurationResponse_gameConfiguration' - Details about the game configuration.
--
-- 'httpStatus', 'updateGameConfigurationResponse_httpStatus' - The response's http status code.
newUpdateGameConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGameConfigurationResponse
newUpdateGameConfigurationResponse pHttpStatus_ =
  UpdateGameConfigurationResponse'
    { gameConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the game configuration.
updateGameConfigurationResponse_gameConfiguration :: Lens.Lens' UpdateGameConfigurationResponse (Prelude.Maybe GameConfigurationDetails)
updateGameConfigurationResponse_gameConfiguration = Lens.lens (\UpdateGameConfigurationResponse' {gameConfiguration} -> gameConfiguration) (\s@UpdateGameConfigurationResponse' {} a -> s {gameConfiguration = a} :: UpdateGameConfigurationResponse)

-- | The response's http status code.
updateGameConfigurationResponse_httpStatus :: Lens.Lens' UpdateGameConfigurationResponse Prelude.Int
updateGameConfigurationResponse_httpStatus = Lens.lens (\UpdateGameConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateGameConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateGameConfigurationResponse)

instance
  Prelude.NFData
    UpdateGameConfigurationResponse
  where
  rnf UpdateGameConfigurationResponse' {..} =
    Prelude.rnf gameConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
