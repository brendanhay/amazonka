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
-- Module      : Amazonka.Transfer.Types.TagStepDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.TagStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.S3Tag

-- | Each step type has its own @StepDetails@ structure.
--
-- The key\/value pairs used to tag a file during the execution of a
-- workflow step.
--
-- /See:/ 'newTagStepDetails' smart constructor.
data TagStepDetails = TagStepDetails'
  { -- | The name of the step, used as an identifier.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies which file to use as input to the workflow step: either the
    -- output from the previous step, or the originally uploaded file for the
    -- workflow.
    --
    -- -   Enter @${previous.file}@ to use the previous file as the input. In
    --     this case, this workflow step uses the output file from the previous
    --     workflow step as input. This is the default value.
    --
    -- -   Enter @${original.file}@ to use the originally-uploaded file
    --     location as input for this step.
    sourceFileLocation :: Prelude.Maybe Prelude.Text,
    -- | Array that contains from 1 to 10 key\/value pairs.
    tags :: Prelude.Maybe (Prelude.NonEmpty S3Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagStepDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'tagStepDetails_name' - The name of the step, used as an identifier.
--
-- 'sourceFileLocation', 'tagStepDetails_sourceFileLocation' - Specifies which file to use as input to the workflow step: either the
-- output from the previous step, or the originally uploaded file for the
-- workflow.
--
-- -   Enter @${previous.file}@ to use the previous file as the input. In
--     this case, this workflow step uses the output file from the previous
--     workflow step as input. This is the default value.
--
-- -   Enter @${original.file}@ to use the originally-uploaded file
--     location as input for this step.
--
-- 'tags', 'tagStepDetails_tags' - Array that contains from 1 to 10 key\/value pairs.
newTagStepDetails ::
  TagStepDetails
newTagStepDetails =
  TagStepDetails'
    { name = Prelude.Nothing,
      sourceFileLocation = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The name of the step, used as an identifier.
tagStepDetails_name :: Lens.Lens' TagStepDetails (Prelude.Maybe Prelude.Text)
tagStepDetails_name = Lens.lens (\TagStepDetails' {name} -> name) (\s@TagStepDetails' {} a -> s {name = a} :: TagStepDetails)

-- | Specifies which file to use as input to the workflow step: either the
-- output from the previous step, or the originally uploaded file for the
-- workflow.
--
-- -   Enter @${previous.file}@ to use the previous file as the input. In
--     this case, this workflow step uses the output file from the previous
--     workflow step as input. This is the default value.
--
-- -   Enter @${original.file}@ to use the originally-uploaded file
--     location as input for this step.
tagStepDetails_sourceFileLocation :: Lens.Lens' TagStepDetails (Prelude.Maybe Prelude.Text)
tagStepDetails_sourceFileLocation = Lens.lens (\TagStepDetails' {sourceFileLocation} -> sourceFileLocation) (\s@TagStepDetails' {} a -> s {sourceFileLocation = a} :: TagStepDetails)

-- | Array that contains from 1 to 10 key\/value pairs.
tagStepDetails_tags :: Lens.Lens' TagStepDetails (Prelude.Maybe (Prelude.NonEmpty S3Tag))
tagStepDetails_tags = Lens.lens (\TagStepDetails' {tags} -> tags) (\s@TagStepDetails' {} a -> s {tags = a} :: TagStepDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TagStepDetails where
  parseJSON =
    Data.withObject
      "TagStepDetails"
      ( \x ->
          TagStepDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SourceFileLocation")
            Prelude.<*> (x Data..:? "Tags")
      )

instance Prelude.Hashable TagStepDetails where
  hashWithSalt _salt TagStepDetails' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceFileLocation
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagStepDetails where
  rnf TagStepDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceFileLocation
      `Prelude.seq` Prelude.rnf tags

instance Data.ToJSON TagStepDetails where
  toJSON TagStepDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("SourceFileLocation" Data..=)
              Prelude.<$> sourceFileLocation,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )
