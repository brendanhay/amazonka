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
-- Module      : Amazonka.GamesParks.CreateStage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new stage for stage-by-stage game development and deployment.
module Amazonka.GamesParks.CreateStage
  ( -- * Creating a Request
    CreateStage (..),
    newCreateStage,

    -- * Request Lenses
    createStage_clientToken,
    createStage_description,
    createStage_tags,
    createStage_gameName,
    createStage_role,
    createStage_stageName,

    -- * Destructuring the Response
    CreateStageResponse (..),
    newCreateStageResponse,

    -- * Response Lenses
    createStageResponse_stage,
    createStageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStage' smart constructor.
data CreateStage = CreateStage'
  { -- | A client-defined token. With an active client token in the request, this
    -- action is idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of tags to apply to the stage.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role to run the game with. This
    -- role can be a game-defined role or the default role that GameSparks
    -- created.
    role' :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createStage_clientToken' - A client-defined token. With an active client token in the request, this
-- action is idempotent.
--
-- 'description', 'createStage_description' - The description of the stage.
--
-- 'tags', 'createStage_tags' - The list of tags to apply to the stage.
--
-- 'gameName', 'createStage_gameName' - The name of the game.
--
-- 'role'', 'createStage_role' - The Amazon Resource Name (ARN) of the role to run the game with. This
-- role can be a game-defined role or the default role that GameSparks
-- created.
--
-- 'stageName', 'createStage_stageName' - The name of the stage.
newCreateStage ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  CreateStage
newCreateStage pGameName_ pRole_ pStageName_ =
  CreateStage'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      gameName = pGameName_,
      role' = pRole_,
      stageName = pStageName_
    }

-- | A client-defined token. With an active client token in the request, this
-- action is idempotent.
createStage_clientToken :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_clientToken = Lens.lens (\CreateStage' {clientToken} -> clientToken) (\s@CreateStage' {} a -> s {clientToken = a} :: CreateStage)

-- | The description of the stage.
createStage_description :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_description = Lens.lens (\CreateStage' {description} -> description) (\s@CreateStage' {} a -> s {description = a} :: CreateStage)

-- | The list of tags to apply to the stage.
createStage_tags :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStage_tags = Lens.lens (\CreateStage' {tags} -> tags) (\s@CreateStage' {} a -> s {tags = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the game.
createStage_gameName :: Lens.Lens' CreateStage Prelude.Text
createStage_gameName = Lens.lens (\CreateStage' {gameName} -> gameName) (\s@CreateStage' {} a -> s {gameName = a} :: CreateStage)

-- | The Amazon Resource Name (ARN) of the role to run the game with. This
-- role can be a game-defined role or the default role that GameSparks
-- created.
createStage_role :: Lens.Lens' CreateStage Prelude.Text
createStage_role = Lens.lens (\CreateStage' {role'} -> role') (\s@CreateStage' {} a -> s {role' = a} :: CreateStage)

-- | The name of the stage.
createStage_stageName :: Lens.Lens' CreateStage Prelude.Text
createStage_stageName = Lens.lens (\CreateStage' {stageName} -> stageName) (\s@CreateStage' {} a -> s {stageName = a} :: CreateStage)

instance Core.AWSRequest CreateStage where
  type AWSResponse CreateStage = CreateStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStageResponse'
            Prelude.<$> (x Data..?> "Stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStage where
  hashWithSalt _salt CreateStage' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData CreateStage where
  rnf CreateStage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders CreateStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStage where
  toJSON CreateStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Role" Data..= role'),
            Prelude.Just ("StageName" Data..= stageName)
          ]
      )

instance Data.ToPath CreateStage where
  toPath CreateStage' {..} =
    Prelude.mconcat
      ["/game/", Data.toBS gameName, "/stage"]

instance Data.ToQuery CreateStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStageResponse' smart constructor.
data CreateStageResponse = CreateStageResponse'
  { -- | Properties that describe the stage.
    stage :: Prelude.Maybe StageDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'createStageResponse_stage' - Properties that describe the stage.
--
-- 'httpStatus', 'createStageResponse_httpStatus' - The response's http status code.
newCreateStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStageResponse
newCreateStageResponse pHttpStatus_ =
  CreateStageResponse'
    { stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties that describe the stage.
createStageResponse_stage :: Lens.Lens' CreateStageResponse (Prelude.Maybe StageDetails)
createStageResponse_stage = Lens.lens (\CreateStageResponse' {stage} -> stage) (\s@CreateStageResponse' {} a -> s {stage = a} :: CreateStageResponse)

-- | The response's http status code.
createStageResponse_httpStatus :: Lens.Lens' CreateStageResponse Prelude.Int
createStageResponse_httpStatus = Lens.lens (\CreateStageResponse' {httpStatus} -> httpStatus) (\s@CreateStageResponse' {} a -> s {httpStatus = a} :: CreateStageResponse)

instance Prelude.NFData CreateStageResponse where
  rnf CreateStageResponse' {..} =
    Prelude.rnf stage
      `Prelude.seq` Prelude.rnf httpStatus
