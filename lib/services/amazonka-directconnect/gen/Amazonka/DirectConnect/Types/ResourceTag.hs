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
-- Module      : Amazonka.DirectConnect.Types.ResourceTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.ResourceTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about a tag associated with an Direct Connect resource.
--
-- /See:/ 'newResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'resourceTag_resourceArn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'tags', 'resourceTag_tags' - The tags.
newResourceTag ::
  ResourceTag
newResourceTag =
  ResourceTag'
    { resourceArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
resourceTag_resourceArn :: Lens.Lens' ResourceTag (Prelude.Maybe Prelude.Text)
resourceTag_resourceArn = Lens.lens (\ResourceTag' {resourceArn} -> resourceArn) (\s@ResourceTag' {} a -> s {resourceArn = a} :: ResourceTag)

-- | The tags.
resourceTag_tags :: Lens.Lens' ResourceTag (Prelude.Maybe (Prelude.NonEmpty Tag))
resourceTag_tags = Lens.lens (\ResourceTag' {tags} -> tags) (\s@ResourceTag' {} a -> s {tags = a} :: ResourceTag) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceTag where
  parseJSON =
    Data.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Prelude.<$> (x Data..:? "resourceArn")
            Prelude.<*> (x Data..:? "tags")
      )

instance Prelude.Hashable ResourceTag where
  hashWithSalt _salt ResourceTag' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ResourceTag where
  rnf ResourceTag' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf tags
