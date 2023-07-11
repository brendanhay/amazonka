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
-- Module      : Amazonka.WAFV2.Types.TagInfoForResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.TagInfoForResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.Tag

-- | The collection of tagging definitions for an Amazon Web Services
-- resource. Tags are key:value pairs that you can use to categorize and
-- manage your resources, for purposes like billing or other management.
-- Typically, the tag key represents a category, such as \"environment\",
-- and the tag value represents a specific value within that category, such
-- as \"test,\" \"development,\" or \"production\". Or you might set the
-- tag key to \"customer\" and the value to the customer name or ID. You
-- can specify one or more tags to add to each Amazon Web Services
-- resource, up to 50 tags for a resource.
--
-- You can tag the Amazon Web Services resources that you manage through
-- WAF: web ACLs, rule groups, IP sets, and regex pattern sets. You can\'t
-- manage or view tags through the WAF console.
--
-- /See:/ 'newTagInfoForResource' smart constructor.
data TagInfoForResource = TagInfoForResource'
  { -- | The Amazon Resource Name (ARN) of the resource.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The array of Tag objects defined for the resource.
    tagList :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagInfoForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'tagInfoForResource_resourceARN' - The Amazon Resource Name (ARN) of the resource.
--
-- 'tagList', 'tagInfoForResource_tagList' - The array of Tag objects defined for the resource.
newTagInfoForResource ::
  TagInfoForResource
newTagInfoForResource =
  TagInfoForResource'
    { resourceARN = Prelude.Nothing,
      tagList = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
tagInfoForResource_resourceARN :: Lens.Lens' TagInfoForResource (Prelude.Maybe Prelude.Text)
tagInfoForResource_resourceARN = Lens.lens (\TagInfoForResource' {resourceARN} -> resourceARN) (\s@TagInfoForResource' {} a -> s {resourceARN = a} :: TagInfoForResource)

-- | The array of Tag objects defined for the resource.
tagInfoForResource_tagList :: Lens.Lens' TagInfoForResource (Prelude.Maybe (Prelude.NonEmpty Tag))
tagInfoForResource_tagList = Lens.lens (\TagInfoForResource' {tagList} -> tagList) (\s@TagInfoForResource' {} a -> s {tagList = a} :: TagInfoForResource) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TagInfoForResource where
  parseJSON =
    Data.withObject
      "TagInfoForResource"
      ( \x ->
          TagInfoForResource'
            Prelude.<$> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "TagList")
      )

instance Prelude.Hashable TagInfoForResource where
  hashWithSalt _salt TagInfoForResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` tagList

instance Prelude.NFData TagInfoForResource where
  rnf TagInfoForResource' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf tagList
