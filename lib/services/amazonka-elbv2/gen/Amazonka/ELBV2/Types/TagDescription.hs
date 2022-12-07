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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | Information about the tags.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'tagDescription_tags' - Information about the tags.
--
-- 'resourceArn', 'tagDescription_resourceArn' - The Amazon Resource Name (ARN) of the resource.
newTagDescription ::
  TagDescription
newTagDescription =
  TagDescription'
    { tags = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | Information about the tags.
tagDescription_tags :: Lens.Lens' TagDescription (Prelude.Maybe (Prelude.NonEmpty Tag))
tagDescription_tags = Lens.lens (\TagDescription' {tags} -> tags) (\s@TagDescription' {} a -> s {tags = a} :: TagDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the resource.
tagDescription_resourceArn :: Lens.Lens' TagDescription (Prelude.Maybe Prelude.Text)
tagDescription_resourceArn = Lens.lens (\TagDescription' {resourceArn} -> resourceArn) (\s@TagDescription' {} a -> s {resourceArn = a} :: TagDescription)

instance Data.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Prelude.<$> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList1 "member")
                  )
      Prelude.<*> (x Data..@? "ResourceArn")

instance Prelude.Hashable TagDescription where
  hashWithSalt _salt TagDescription' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData TagDescription where
  rnf TagDescription' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceArn
