{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags where

import Network.AWS.CloudFront.Types.StreamingDistributionConfig
import Network.AWS.CloudFront.Types.Tags
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A streaming distribution Configuration and a list of tags to be
-- associated with the streaming distribution.
--
-- /See:/ 'newStreamingDistributionConfigWithTags' smart constructor.
data StreamingDistributionConfigWithTags = StreamingDistributionConfigWithTags'
  { -- | A streaming distribution Configuration.
    streamingDistributionConfig :: StreamingDistributionConfig,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StreamingDistributionConfigWithTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingDistributionConfig', 'streamingDistributionConfigWithTags_streamingDistributionConfig' - A streaming distribution Configuration.
--
-- 'tags', 'streamingDistributionConfigWithTags_tags' - A complex type that contains zero or more @Tag@ elements.
newStreamingDistributionConfigWithTags ::
  -- | 'streamingDistributionConfig'
  StreamingDistributionConfig ->
  -- | 'tags'
  Tags ->
  StreamingDistributionConfigWithTags
newStreamingDistributionConfigWithTags
  pStreamingDistributionConfig_
  pTags_ =
    StreamingDistributionConfigWithTags'
      { streamingDistributionConfig =
          pStreamingDistributionConfig_,
        tags = pTags_
      }

-- | A streaming distribution Configuration.
streamingDistributionConfigWithTags_streamingDistributionConfig :: Lens.Lens' StreamingDistributionConfigWithTags StreamingDistributionConfig
streamingDistributionConfigWithTags_streamingDistributionConfig = Lens.lens (\StreamingDistributionConfigWithTags' {streamingDistributionConfig} -> streamingDistributionConfig) (\s@StreamingDistributionConfigWithTags' {} a -> s {streamingDistributionConfig = a} :: StreamingDistributionConfigWithTags)

-- | A complex type that contains zero or more @Tag@ elements.
streamingDistributionConfigWithTags_tags :: Lens.Lens' StreamingDistributionConfigWithTags Tags
streamingDistributionConfigWithTags_tags = Lens.lens (\StreamingDistributionConfigWithTags' {tags} -> tags) (\s@StreamingDistributionConfigWithTags' {} a -> s {tags = a} :: StreamingDistributionConfigWithTags)

instance
  Prelude.Hashable
    StreamingDistributionConfigWithTags

instance
  Prelude.NFData
    StreamingDistributionConfigWithTags

instance
  Prelude.ToXML
    StreamingDistributionConfigWithTags
  where
  toXML StreamingDistributionConfigWithTags' {..} =
    Prelude.mconcat
      [ "StreamingDistributionConfig"
          Prelude.@= streamingDistributionConfig,
        "Tags" Prelude.@= tags
      ]
