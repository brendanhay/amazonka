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
-- Module      : Amazonka.IVSRealtime.CreateStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new stage (and optionally participant tokens).
module Amazonka.IVSRealtime.CreateStage
  ( -- * Creating a Request
    CreateStage (..),
    newCreateStage,

    -- * Request Lenses
    createStage_name,
    createStage_participantTokenConfigurations,
    createStage_tags,

    -- * Destructuring the Response
    CreateStageResponse (..),
    newCreateStageResponse,

    -- * Response Lenses
    createStageResponse_participantTokens,
    createStageResponse_stage,
    createStageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStage' smart constructor.
data CreateStage = CreateStage'
  { -- | Optional name that can be specified for the stage being created.
    name :: Prelude.Maybe Prelude.Text,
    -- | Array of participant token configuration objects to attach to the new
    -- stage.
    participantTokenConfigurations :: Prelude.Maybe [ParticipantTokenConfiguration],
    -- | Tags attached to the resource. Array of maps, each of the form
    -- @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- for details, including restrictions that apply to tags and \"Tag naming
    -- limits and requirements\"; Amazon IVS has no constraints on tags beyond
    -- what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'name', 'createStage_name' - Optional name that can be specified for the stage being created.
--
-- 'participantTokenConfigurations', 'createStage_participantTokenConfigurations' - Array of participant token configuration objects to attach to the new
-- stage.
--
-- 'tags', 'createStage_tags' - Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS has no constraints on tags beyond
-- what is documented there.
newCreateStage ::
  CreateStage
newCreateStage =
  CreateStage'
    { name = Prelude.Nothing,
      participantTokenConfigurations = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Optional name that can be specified for the stage being created.
createStage_name :: Lens.Lens' CreateStage (Prelude.Maybe Prelude.Text)
createStage_name = Lens.lens (\CreateStage' {name} -> name) (\s@CreateStage' {} a -> s {name = a} :: CreateStage)

-- | Array of participant token configuration objects to attach to the new
-- stage.
createStage_participantTokenConfigurations :: Lens.Lens' CreateStage (Prelude.Maybe [ParticipantTokenConfiguration])
createStage_participantTokenConfigurations = Lens.lens (\CreateStage' {participantTokenConfigurations} -> participantTokenConfigurations) (\s@CreateStage' {} a -> s {participantTokenConfigurations = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

-- | Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS has no constraints on tags beyond
-- what is documented there.
createStage_tags :: Lens.Lens' CreateStage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStage_tags = Lens.lens (\CreateStage' {tags} -> tags) (\s@CreateStage' {} a -> s {tags = a} :: CreateStage) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateStage where
  type AWSResponse CreateStage = CreateStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStageResponse'
            Prelude.<$> ( x
                            Data..?> "participantTokens"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStage where
  hashWithSalt _salt CreateStage' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` participantTokenConfigurations
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateStage where
  rnf CreateStage' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf participantTokenConfigurations
      `Prelude.seq` Prelude.rnf tags

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
          [ ("name" Data..=) Prelude.<$> name,
            ("participantTokenConfigurations" Data..=)
              Prelude.<$> participantTokenConfigurations,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateStage where
  toPath = Prelude.const "/CreateStage"

instance Data.ToQuery CreateStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStageResponse' smart constructor.
data CreateStageResponse = CreateStageResponse'
  { -- | Participant tokens attached to the stage. These correspond to the
    -- @participants@ in the request.
    participantTokens :: Prelude.Maybe [ParticipantToken],
    -- | The stage that was created.
    stage :: Prelude.Maybe Stage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantTokens', 'createStageResponse_participantTokens' - Participant tokens attached to the stage. These correspond to the
-- @participants@ in the request.
--
-- 'stage', 'createStageResponse_stage' - The stage that was created.
--
-- 'httpStatus', 'createStageResponse_httpStatus' - The response's http status code.
newCreateStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStageResponse
newCreateStageResponse pHttpStatus_ =
  CreateStageResponse'
    { participantTokens =
        Prelude.Nothing,
      stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Participant tokens attached to the stage. These correspond to the
-- @participants@ in the request.
createStageResponse_participantTokens :: Lens.Lens' CreateStageResponse (Prelude.Maybe [ParticipantToken])
createStageResponse_participantTokens = Lens.lens (\CreateStageResponse' {participantTokens} -> participantTokens) (\s@CreateStageResponse' {} a -> s {participantTokens = a} :: CreateStageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The stage that was created.
createStageResponse_stage :: Lens.Lens' CreateStageResponse (Prelude.Maybe Stage)
createStageResponse_stage = Lens.lens (\CreateStageResponse' {stage} -> stage) (\s@CreateStageResponse' {} a -> s {stage = a} :: CreateStageResponse)

-- | The response's http status code.
createStageResponse_httpStatus :: Lens.Lens' CreateStageResponse Prelude.Int
createStageResponse_httpStatus = Lens.lens (\CreateStageResponse' {httpStatus} -> httpStatus) (\s@CreateStageResponse' {} a -> s {httpStatus = a} :: CreateStageResponse)

instance Prelude.NFData CreateStageResponse where
  rnf CreateStageResponse' {..} =
    Prelude.rnf participantTokens
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf httpStatus
