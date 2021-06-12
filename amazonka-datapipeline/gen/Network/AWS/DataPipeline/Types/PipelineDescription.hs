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
-- Module      : Network.AWS.DataPipeline.Types.PipelineDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types.Field
import Network.AWS.DataPipeline.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Contains pipeline metadata.
--
-- /See:/ 'newPipelineDescription' smart constructor.
data PipelineDescription = PipelineDescription'
  { -- | A list of tags to associated with a pipeline. Tags let you control
    -- access to pipelines. For more information, see
    -- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
    -- in the /AWS Data Pipeline Developer Guide/.
    tags :: Core.Maybe [Tag],
    -- | Description of the pipeline.
    description :: Core.Maybe Core.Text,
    -- | The pipeline identifier that was assigned by AWS Data Pipeline. This is
    -- a string of the form @df-297EG78HU43EEXAMPLE@.
    pipelineId :: Core.Text,
    -- | The name of the pipeline.
    name :: Core.Text,
    -- | A list of read-only fields that contain metadata about the pipeline:
    -- \@userId, \@accountId, and \@pipelineState.
    fields :: [Field]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'pipelineDescription_tags' - A list of tags to associated with a pipeline. Tags let you control
-- access to pipelines. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
-- in the /AWS Data Pipeline Developer Guide/.
--
-- 'description', 'pipelineDescription_description' - Description of the pipeline.
--
-- 'pipelineId', 'pipelineDescription_pipelineId' - The pipeline identifier that was assigned by AWS Data Pipeline. This is
-- a string of the form @df-297EG78HU43EEXAMPLE@.
--
-- 'name', 'pipelineDescription_name' - The name of the pipeline.
--
-- 'fields', 'pipelineDescription_fields' - A list of read-only fields that contain metadata about the pipeline:
-- \@userId, \@accountId, and \@pipelineState.
newPipelineDescription ::
  -- | 'pipelineId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  PipelineDescription
newPipelineDescription pPipelineId_ pName_ =
  PipelineDescription'
    { tags = Core.Nothing,
      description = Core.Nothing,
      pipelineId = pPipelineId_,
      name = pName_,
      fields = Core.mempty
    }

-- | A list of tags to associated with a pipeline. Tags let you control
-- access to pipelines. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
-- in the /AWS Data Pipeline Developer Guide/.
pipelineDescription_tags :: Lens.Lens' PipelineDescription (Core.Maybe [Tag])
pipelineDescription_tags = Lens.lens (\PipelineDescription' {tags} -> tags) (\s@PipelineDescription' {} a -> s {tags = a} :: PipelineDescription) Core.. Lens.mapping Lens._Coerce

-- | Description of the pipeline.
pipelineDescription_description :: Lens.Lens' PipelineDescription (Core.Maybe Core.Text)
pipelineDescription_description = Lens.lens (\PipelineDescription' {description} -> description) (\s@PipelineDescription' {} a -> s {description = a} :: PipelineDescription)

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is
-- a string of the form @df-297EG78HU43EEXAMPLE@.
pipelineDescription_pipelineId :: Lens.Lens' PipelineDescription Core.Text
pipelineDescription_pipelineId = Lens.lens (\PipelineDescription' {pipelineId} -> pipelineId) (\s@PipelineDescription' {} a -> s {pipelineId = a} :: PipelineDescription)

-- | The name of the pipeline.
pipelineDescription_name :: Lens.Lens' PipelineDescription Core.Text
pipelineDescription_name = Lens.lens (\PipelineDescription' {name} -> name) (\s@PipelineDescription' {} a -> s {name = a} :: PipelineDescription)

-- | A list of read-only fields that contain metadata about the pipeline:
-- \@userId, \@accountId, and \@pipelineState.
pipelineDescription_fields :: Lens.Lens' PipelineDescription [Field]
pipelineDescription_fields = Lens.lens (\PipelineDescription' {fields} -> fields) (\s@PipelineDescription' {} a -> s {fields = a} :: PipelineDescription) Core.. Lens._Coerce

instance Core.FromJSON PipelineDescription where
  parseJSON =
    Core.withObject
      "PipelineDescription"
      ( \x ->
          PipelineDescription'
            Core.<$> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..: "pipelineId")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..:? "fields" Core..!= Core.mempty)
      )

instance Core.Hashable PipelineDescription

instance Core.NFData PipelineDescription
