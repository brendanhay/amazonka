{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags where

import Network.AWS.CloudFront.Types.StreamingDistributionConfig
import Network.AWS.CloudFront.Types.Tags
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A streaming distribution Configuration and a list of tags to be associated with the streaming distribution.
--
--
--
-- /See:/ 'streamingDistributionConfigWithTags' smart constructor.
data StreamingDistributionConfigWithTags = StreamingDistributionConfigWithTags'
  { _sdcwtStreamingDistributionConfig ::
      !StreamingDistributionConfig,
    _sdcwtTags :: !Tags
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamingDistributionConfigWithTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcwtStreamingDistributionConfig' - A streaming distribution Configuration.
--
-- * 'sdcwtTags' - A complex type that contains zero or more @Tag@ elements.
streamingDistributionConfigWithTags ::
  -- | 'sdcwtStreamingDistributionConfig'
  StreamingDistributionConfig ->
  -- | 'sdcwtTags'
  Tags ->
  StreamingDistributionConfigWithTags
streamingDistributionConfigWithTags
  pStreamingDistributionConfig_
  pTags_ =
    StreamingDistributionConfigWithTags'
      { _sdcwtStreamingDistributionConfig =
          pStreamingDistributionConfig_,
        _sdcwtTags = pTags_
      }

-- | A streaming distribution Configuration.
sdcwtStreamingDistributionConfig :: Lens' StreamingDistributionConfigWithTags StreamingDistributionConfig
sdcwtStreamingDistributionConfig = lens _sdcwtStreamingDistributionConfig (\s a -> s {_sdcwtStreamingDistributionConfig = a})

-- | A complex type that contains zero or more @Tag@ elements.
sdcwtTags :: Lens' StreamingDistributionConfigWithTags Tags
sdcwtTags = lens _sdcwtTags (\s a -> s {_sdcwtTags = a})

instance Hashable StreamingDistributionConfigWithTags

instance NFData StreamingDistributionConfigWithTags

instance ToXML StreamingDistributionConfigWithTags where
  toXML StreamingDistributionConfigWithTags' {..} =
    mconcat
      [ "StreamingDistributionConfig"
          @= _sdcwtStreamingDistributionConfig,
        "Tags" @= _sdcwtTags
      ]
