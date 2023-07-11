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
-- Module      : Amazonka.WAF.Types.TagInfoForResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.TagInfoForResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAF.Types.Tag

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Information for a tag associated with an AWS resource. Tags are
-- key:value pairs that you can use to categorize and manage your
-- resources, for purposes like billing. For example, you might set the tag
-- key to \"customer\" and the value to the customer name or ID. You can
-- specify one or more tags to add to each AWS resource, up to 50 tags for
-- a resource.
--
-- Tagging is only available through the API, SDKs, and CLI. You can\'t
-- manage or view tags through the AWS WAF Classic console. You can tag the
-- AWS resources that you manage through AWS WAF Classic: web ACLs, rule
-- groups, and rules.
--
-- /See:/ 'newTagInfoForResource' smart constructor.
data TagInfoForResource = TagInfoForResource'
  { resourceARN :: Prelude.Maybe Prelude.Text,
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
-- 'resourceARN', 'tagInfoForResource_resourceARN' -
--
-- 'tagList', 'tagInfoForResource_tagList' -
newTagInfoForResource ::
  TagInfoForResource
newTagInfoForResource =
  TagInfoForResource'
    { resourceARN = Prelude.Nothing,
      tagList = Prelude.Nothing
    }

tagInfoForResource_resourceARN :: Lens.Lens' TagInfoForResource (Prelude.Maybe Prelude.Text)
tagInfoForResource_resourceARN = Lens.lens (\TagInfoForResource' {resourceARN} -> resourceARN) (\s@TagInfoForResource' {} a -> s {resourceARN = a} :: TagInfoForResource)

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
