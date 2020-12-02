{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionConfigWithTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionConfigWithTags where

import Network.AWS.CloudFront.Types.DistributionConfig
import Network.AWS.CloudFront.Types.Tags
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A distribution Configuration and a list of tags to be associated with the distribution.
--
--
--
-- /See:/ 'distributionConfigWithTags' smart constructor.
data DistributionConfigWithTags = DistributionConfigWithTags'
  { _dcwtDistributionConfig ::
      !DistributionConfig,
    _dcwtTags :: !Tags
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DistributionConfigWithTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcwtDistributionConfig' - A distribution configuration.
--
-- * 'dcwtTags' - A complex type that contains zero or more @Tag@ elements.
distributionConfigWithTags ::
  -- | 'dcwtDistributionConfig'
  DistributionConfig ->
  -- | 'dcwtTags'
  Tags ->
  DistributionConfigWithTags
distributionConfigWithTags pDistributionConfig_ pTags_ =
  DistributionConfigWithTags'
    { _dcwtDistributionConfig =
        pDistributionConfig_,
      _dcwtTags = pTags_
    }

-- | A distribution configuration.
dcwtDistributionConfig :: Lens' DistributionConfigWithTags DistributionConfig
dcwtDistributionConfig = lens _dcwtDistributionConfig (\s a -> s {_dcwtDistributionConfig = a})

-- | A complex type that contains zero or more @Tag@ elements.
dcwtTags :: Lens' DistributionConfigWithTags Tags
dcwtTags = lens _dcwtTags (\s a -> s {_dcwtTags = a})

instance Hashable DistributionConfigWithTags

instance NFData DistributionConfigWithTags

instance ToXML DistributionConfigWithTags where
  toXML DistributionConfigWithTags' {..} =
    mconcat
      [ "DistributionConfig" @= _dcwtDistributionConfig,
        "Tags" @= _dcwtTags
      ]
