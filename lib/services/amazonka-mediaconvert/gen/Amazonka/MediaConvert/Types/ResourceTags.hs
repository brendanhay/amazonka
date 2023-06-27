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
-- Module      : Amazonka.MediaConvert.Types.ResourceTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ResourceTags where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Resource Name (ARN) and tags for an AWS Elemental
-- MediaConvert resource.
--
-- /See:/ 'newResourceTags' smart constructor.
data ResourceTags = ResourceTags'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The tags for the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceTags_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'tags', 'resourceTags_tags' - The tags for the resource.
newResourceTags ::
  ResourceTags
newResourceTags =
  ResourceTags'
    { arn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
resourceTags_arn :: Lens.Lens' ResourceTags (Prelude.Maybe Prelude.Text)
resourceTags_arn = Lens.lens (\ResourceTags' {arn} -> arn) (\s@ResourceTags' {} a -> s {arn = a} :: ResourceTags)

-- | The tags for the resource.
resourceTags_tags :: Lens.Lens' ResourceTags (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resourceTags_tags = Lens.lens (\ResourceTags' {tags} -> tags) (\s@ResourceTags' {} a -> s {tags = a} :: ResourceTags) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceTags where
  parseJSON =
    Data.withObject
      "ResourceTags"
      ( \x ->
          ResourceTags'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceTags where
  hashWithSalt _salt ResourceTags' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ResourceTags where
  rnf ResourceTags' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf tags
