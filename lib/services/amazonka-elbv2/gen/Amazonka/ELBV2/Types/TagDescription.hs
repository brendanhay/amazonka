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
-- Module      : Amazonka.ELBV2.Types.TagDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.TagDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The tags associated with a resource.
--
-- /See:/ 'newTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the tags.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'tagDescription_resourceArn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'tags', 'tagDescription_tags' - Information about the tags.
newTagDescription ::
  TagDescription
newTagDescription =
  TagDescription'
    { resourceArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
tagDescription_resourceArn :: Lens.Lens' TagDescription (Prelude.Maybe Prelude.Text)
tagDescription_resourceArn = Lens.lens (\TagDescription' {resourceArn} -> resourceArn) (\s@TagDescription' {} a -> s {resourceArn = a} :: TagDescription)

-- | Information about the tags.
tagDescription_tags :: Lens.Lens' TagDescription (Prelude.Maybe (Prelude.NonEmpty Tag))
tagDescription_tags = Lens.lens (\TagDescription' {tags} -> tags) (\s@TagDescription' {} a -> s {tags = a} :: TagDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Prelude.<$> (x Data..@? "ResourceArn")
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList1 "member")
                  )

instance Prelude.Hashable TagDescription where
  hashWithSalt _salt TagDescription' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagDescription where
  rnf TagDescription' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf tags
