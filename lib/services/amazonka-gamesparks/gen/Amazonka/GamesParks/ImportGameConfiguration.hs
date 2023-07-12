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
-- Module      : Amazonka.GamesParks.ImportGameConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a game configuration.
--
-- This operation replaces the current configuration of the game with the
-- provided input. This is not a reversible operation. If you want to
-- preserve the previous configuration, use @CreateSnapshot@ to make a new
-- snapshot before importing.
module Amazonka.GamesParks.ImportGameConfiguration
  ( -- * Creating a Request
    ImportGameConfiguration (..),
    newImportGameConfiguration,

    -- * Request Lenses
    importGameConfiguration_gameName,
    importGameConfiguration_importSource,

    -- * Destructuring the Response
    ImportGameConfigurationResponse (..),
    newImportGameConfigurationResponse,

    -- * Response Lenses
    importGameConfigurationResponse_gameConfiguration,
    importGameConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportGameConfiguration' smart constructor.
data ImportGameConfiguration = ImportGameConfiguration'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The source used to import configuration sections.
    importSource :: ImportGameConfigurationSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportGameConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'importGameConfiguration_gameName' - The name of the game.
--
-- 'importSource', 'importGameConfiguration_importSource' - The source used to import configuration sections.
newImportGameConfiguration ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'importSource'
  ImportGameConfigurationSource ->
  ImportGameConfiguration
newImportGameConfiguration pGameName_ pImportSource_ =
  ImportGameConfiguration'
    { gameName = pGameName_,
      importSource = pImportSource_
    }

-- | The name of the game.
importGameConfiguration_gameName :: Lens.Lens' ImportGameConfiguration Prelude.Text
importGameConfiguration_gameName = Lens.lens (\ImportGameConfiguration' {gameName} -> gameName) (\s@ImportGameConfiguration' {} a -> s {gameName = a} :: ImportGameConfiguration)

-- | The source used to import configuration sections.
importGameConfiguration_importSource :: Lens.Lens' ImportGameConfiguration ImportGameConfigurationSource
importGameConfiguration_importSource = Lens.lens (\ImportGameConfiguration' {importSource} -> importSource) (\s@ImportGameConfiguration' {} a -> s {importSource = a} :: ImportGameConfiguration)

instance Core.AWSRequest ImportGameConfiguration where
  type
    AWSResponse ImportGameConfiguration =
      ImportGameConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportGameConfigurationResponse'
            Prelude.<$> (x Data..?> "GameConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportGameConfiguration where
  hashWithSalt _salt ImportGameConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` importSource

instance Prelude.NFData ImportGameConfiguration where
  rnf ImportGameConfiguration' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf importSource

instance Data.ToHeaders ImportGameConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportGameConfiguration where
  toJSON ImportGameConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImportSource" Data..= importSource)]
      )

instance Data.ToPath ImportGameConfiguration where
  toPath ImportGameConfiguration' {..} =
    Prelude.mconcat
      ["/game/", Data.toBS gameName, "/configuration"]

instance Data.ToQuery ImportGameConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportGameConfigurationResponse' smart constructor.
data ImportGameConfigurationResponse = ImportGameConfigurationResponse'
  { -- | Details about the game configuration.
    gameConfiguration :: Prelude.Maybe GameConfigurationDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportGameConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameConfiguration', 'importGameConfigurationResponse_gameConfiguration' - Details about the game configuration.
--
-- 'httpStatus', 'importGameConfigurationResponse_httpStatus' - The response's http status code.
newImportGameConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportGameConfigurationResponse
newImportGameConfigurationResponse pHttpStatus_ =
  ImportGameConfigurationResponse'
    { gameConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the game configuration.
importGameConfigurationResponse_gameConfiguration :: Lens.Lens' ImportGameConfigurationResponse (Prelude.Maybe GameConfigurationDetails)
importGameConfigurationResponse_gameConfiguration = Lens.lens (\ImportGameConfigurationResponse' {gameConfiguration} -> gameConfiguration) (\s@ImportGameConfigurationResponse' {} a -> s {gameConfiguration = a} :: ImportGameConfigurationResponse)

-- | The response's http status code.
importGameConfigurationResponse_httpStatus :: Lens.Lens' ImportGameConfigurationResponse Prelude.Int
importGameConfigurationResponse_httpStatus = Lens.lens (\ImportGameConfigurationResponse' {httpStatus} -> httpStatus) (\s@ImportGameConfigurationResponse' {} a -> s {httpStatus = a} :: ImportGameConfigurationResponse)

instance
  Prelude.NFData
    ImportGameConfigurationResponse
  where
  rnf ImportGameConfigurationResponse' {..} =
    Prelude.rnf gameConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
