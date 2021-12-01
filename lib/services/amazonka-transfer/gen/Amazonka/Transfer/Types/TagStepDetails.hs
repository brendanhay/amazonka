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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.TagStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
-- 'tags', 'tagStepDetails_tags' - Array that contains from 1 to 10 key\/value pairs.
newTagStepDetails ::
  TagStepDetails
newTagStepDetails =
  TagStepDetails'
    { name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The name of the step, used as an identifier.
tagStepDetails_name :: Lens.Lens' TagStepDetails (Prelude.Maybe Prelude.Text)
tagStepDetails_name = Lens.lens (\TagStepDetails' {name} -> name) (\s@TagStepDetails' {} a -> s {name = a} :: TagStepDetails)

-- | Array that contains from 1 to 10 key\/value pairs.
tagStepDetails_tags :: Lens.Lens' TagStepDetails (Prelude.Maybe (Prelude.NonEmpty S3Tag))
tagStepDetails_tags = Lens.lens (\TagStepDetails' {tags} -> tags) (\s@TagStepDetails' {} a -> s {tags = a} :: TagStepDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TagStepDetails where
  parseJSON =
    Core.withObject
      "TagStepDetails"
      ( \x ->
          TagStepDetails'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Tags")
      )

instance Prelude.Hashable TagStepDetails where
  hashWithSalt salt' TagStepDetails' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData TagStepDetails where
  rnf TagStepDetails' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf tags

instance Core.ToJSON TagStepDetails where
  toJSON TagStepDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Tags" Core..=) Prelude.<$> tags
          ]
      )
