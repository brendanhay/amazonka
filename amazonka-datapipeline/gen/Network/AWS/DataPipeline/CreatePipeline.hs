{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DataPipeline.CreatePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty pipeline. Use PutPipelineDefinition to populate the
-- pipeline.
module Network.AWS.DataPipeline.CreatePipeline
  ( -- * Creating a Request
    CreatePipeline (..),
    newCreatePipeline,

    -- * Request Lenses
    createPipeline_tags,
    createPipeline_description,
    createPipeline_name,
    createPipeline_uniqueId,

    -- * Destructuring the Response
    CreatePipelineResponse (..),
    newCreatePipelineResponse,

    -- * Response Lenses
    createPipelineResponse_httpStatus,
    createPipelineResponse_pipelineId,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreatePipeline.
--
-- /See:/ 'newCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | A list of tags to associate with the pipeline at creation. Tags let you
    -- control access to pipelines. For more information, see
    -- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
    -- in the /AWS Data Pipeline Developer Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The description for the pipeline.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name for the pipeline. You can use the same name for multiple
    -- pipelines associated with your AWS account, because AWS Data Pipeline
    -- assigns each pipeline a unique pipeline identifier.
    name :: Prelude.Text,
    -- | A unique identifier. This identifier is not the same as the pipeline
    -- identifier assigned by AWS Data Pipeline. You are responsible for
    -- defining the format and ensuring the uniqueness of this identifier. You
    -- use this parameter to ensure idempotency during repeated calls to
    -- @CreatePipeline@. For example, if the first call to @CreatePipeline@
    -- does not succeed, you can pass in the same unique identifier and
    -- pipeline name combination on a subsequent call to @CreatePipeline@.
    -- @CreatePipeline@ ensures that if a pipeline already exists with the same
    -- name and unique identifier, a new pipeline is not created. Instead,
    -- you\'ll receive the pipeline identifier from the previous attempt. The
    -- uniqueness of the name and unique identifier combination is scoped to
    -- the AWS account or IAM user credentials.
    uniqueId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPipeline_tags' - A list of tags to associate with the pipeline at creation. Tags let you
-- control access to pipelines. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
-- in the /AWS Data Pipeline Developer Guide/.
--
-- 'description', 'createPipeline_description' - The description for the pipeline.
--
-- 'name', 'createPipeline_name' - The name for the pipeline. You can use the same name for multiple
-- pipelines associated with your AWS account, because AWS Data Pipeline
-- assigns each pipeline a unique pipeline identifier.
--
-- 'uniqueId', 'createPipeline_uniqueId' - A unique identifier. This identifier is not the same as the pipeline
-- identifier assigned by AWS Data Pipeline. You are responsible for
-- defining the format and ensuring the uniqueness of this identifier. You
-- use this parameter to ensure idempotency during repeated calls to
-- @CreatePipeline@. For example, if the first call to @CreatePipeline@
-- does not succeed, you can pass in the same unique identifier and
-- pipeline name combination on a subsequent call to @CreatePipeline@.
-- @CreatePipeline@ ensures that if a pipeline already exists with the same
-- name and unique identifier, a new pipeline is not created. Instead,
-- you\'ll receive the pipeline identifier from the previous attempt. The
-- uniqueness of the name and unique identifier combination is scoped to
-- the AWS account or IAM user credentials.
newCreatePipeline ::
  -- | 'name'
  Prelude.Text ->
  -- | 'uniqueId'
  Prelude.Text ->
  CreatePipeline
newCreatePipeline pName_ pUniqueId_ =
  CreatePipeline'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      uniqueId = pUniqueId_
    }

-- | A list of tags to associate with the pipeline at creation. Tags let you
-- control access to pipelines. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
-- in the /AWS Data Pipeline Developer Guide/.
createPipeline_tags :: Lens.Lens' CreatePipeline (Prelude.Maybe [Tag])
createPipeline_tags = Lens.lens (\CreatePipeline' {tags} -> tags) (\s@CreatePipeline' {} a -> s {tags = a} :: CreatePipeline) Prelude.. Lens.mapping Prelude._Coerce

-- | The description for the pipeline.
createPipeline_description :: Lens.Lens' CreatePipeline (Prelude.Maybe Prelude.Text)
createPipeline_description = Lens.lens (\CreatePipeline' {description} -> description) (\s@CreatePipeline' {} a -> s {description = a} :: CreatePipeline)

-- | The name for the pipeline. You can use the same name for multiple
-- pipelines associated with your AWS account, because AWS Data Pipeline
-- assigns each pipeline a unique pipeline identifier.
createPipeline_name :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_name = Lens.lens (\CreatePipeline' {name} -> name) (\s@CreatePipeline' {} a -> s {name = a} :: CreatePipeline)

-- | A unique identifier. This identifier is not the same as the pipeline
-- identifier assigned by AWS Data Pipeline. You are responsible for
-- defining the format and ensuring the uniqueness of this identifier. You
-- use this parameter to ensure idempotency during repeated calls to
-- @CreatePipeline@. For example, if the first call to @CreatePipeline@
-- does not succeed, you can pass in the same unique identifier and
-- pipeline name combination on a subsequent call to @CreatePipeline@.
-- @CreatePipeline@ ensures that if a pipeline already exists with the same
-- name and unique identifier, a new pipeline is not created. Instead,
-- you\'ll receive the pipeline identifier from the previous attempt. The
-- uniqueness of the name and unique identifier combination is scoped to
-- the AWS account or IAM user credentials.
createPipeline_uniqueId :: Lens.Lens' CreatePipeline Prelude.Text
createPipeline_uniqueId = Lens.lens (\CreatePipeline' {uniqueId} -> uniqueId) (\s@CreatePipeline' {} a -> s {uniqueId = a} :: CreatePipeline)

instance Prelude.AWSRequest CreatePipeline where
  type Rs CreatePipeline = CreatePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "pipelineId")
      )

instance Prelude.Hashable CreatePipeline

instance Prelude.NFData CreatePipeline

instance Prelude.ToHeaders CreatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DataPipeline.CreatePipeline" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePipeline where
  toJSON CreatePipeline' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("tags" Prelude..=) Prelude.<$> tags,
            ("description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("uniqueId" Prelude..= uniqueId)
          ]
      )

instance Prelude.ToPath CreatePipeline where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of CreatePipeline.
--
-- /See:/ 'newCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID that AWS Data Pipeline assigns the newly created pipeline. For
    -- example, @df-06372391ZG65EXAMPLE@.
    pipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPipelineResponse_httpStatus' - The response's http status code.
--
-- 'pipelineId', 'createPipelineResponse_pipelineId' - The ID that AWS Data Pipeline assigns the newly created pipeline. For
-- example, @df-06372391ZG65EXAMPLE@.
newCreatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'pipelineId'
  Prelude.Text ->
  CreatePipelineResponse
newCreatePipelineResponse pHttpStatus_ pPipelineId_ =
  CreatePipelineResponse'
    { httpStatus = pHttpStatus_,
      pipelineId = pPipelineId_
    }

-- | The response's http status code.
createPipelineResponse_httpStatus :: Lens.Lens' CreatePipelineResponse Prelude.Int
createPipelineResponse_httpStatus = Lens.lens (\CreatePipelineResponse' {httpStatus} -> httpStatus) (\s@CreatePipelineResponse' {} a -> s {httpStatus = a} :: CreatePipelineResponse)

-- | The ID that AWS Data Pipeline assigns the newly created pipeline. For
-- example, @df-06372391ZG65EXAMPLE@.
createPipelineResponse_pipelineId :: Lens.Lens' CreatePipelineResponse Prelude.Text
createPipelineResponse_pipelineId = Lens.lens (\CreatePipelineResponse' {pipelineId} -> pipelineId) (\s@CreatePipelineResponse' {} a -> s {pipelineId = a} :: CreatePipelineResponse)

instance Prelude.NFData CreatePipelineResponse
