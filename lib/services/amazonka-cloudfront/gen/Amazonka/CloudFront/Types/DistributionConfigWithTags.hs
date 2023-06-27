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
-- Module      : Amazonka.CloudFront.Types.DistributionConfigWithTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.DistributionConfigWithTags where

import Amazonka.CloudFront.Types.DistributionConfig
import Amazonka.CloudFront.Types.Tags
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A distribution Configuration and a list of tags to be associated with
-- the distribution.
--
-- /See:/ 'newDistributionConfigWithTags' smart constructor.
data DistributionConfigWithTags = DistributionConfigWithTags'
  { -- | A distribution configuration.
    distributionConfig :: DistributionConfig,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributionConfigWithTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionConfig', 'distributionConfigWithTags_distributionConfig' - A distribution configuration.
--
-- 'tags', 'distributionConfigWithTags_tags' - A complex type that contains zero or more @Tag@ elements.
newDistributionConfigWithTags ::
  -- | 'distributionConfig'
  DistributionConfig ->
  -- | 'tags'
  Tags ->
  DistributionConfigWithTags
newDistributionConfigWithTags
  pDistributionConfig_
  pTags_ =
    DistributionConfigWithTags'
      { distributionConfig =
          pDistributionConfig_,
        tags = pTags_
      }

-- | A distribution configuration.
distributionConfigWithTags_distributionConfig :: Lens.Lens' DistributionConfigWithTags DistributionConfig
distributionConfigWithTags_distributionConfig = Lens.lens (\DistributionConfigWithTags' {distributionConfig} -> distributionConfig) (\s@DistributionConfigWithTags' {} a -> s {distributionConfig = a} :: DistributionConfigWithTags)

-- | A complex type that contains zero or more @Tag@ elements.
distributionConfigWithTags_tags :: Lens.Lens' DistributionConfigWithTags Tags
distributionConfigWithTags_tags = Lens.lens (\DistributionConfigWithTags' {tags} -> tags) (\s@DistributionConfigWithTags' {} a -> s {tags = a} :: DistributionConfigWithTags)

instance Prelude.Hashable DistributionConfigWithTags where
  hashWithSalt _salt DistributionConfigWithTags' {..} =
    _salt
      `Prelude.hashWithSalt` distributionConfig
      `Prelude.hashWithSalt` tags

instance Prelude.NFData DistributionConfigWithTags where
  rnf DistributionConfigWithTags' {..} =
    Prelude.rnf distributionConfig
      `Prelude.seq` Prelude.rnf tags

instance Data.ToXML DistributionConfigWithTags where
  toXML DistributionConfigWithTags' {..} =
    Prelude.mconcat
      [ "DistributionConfig" Data.@= distributionConfig,
        "Tags" Data.@= tags
      ]
