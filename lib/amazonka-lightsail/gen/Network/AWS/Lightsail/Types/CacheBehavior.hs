{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CacheBehavior where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.BehaviorEnum
import Network.AWS.Prelude

-- | Describes the default cache behavior of an Amazon Lightsail content delivery network (CDN) distribution.
--
--
--
-- /See:/ 'cacheBehavior' smart constructor.
newtype CacheBehavior = CacheBehavior'
  { _cbBehavior ::
      Maybe BehaviorEnum
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbBehavior' - The cache behavior of the distribution. The following cache behaviors can be specified:     * __@cache@ __ - This option is best for static sites. When specified, your distribution caches and serves your entire website as static content. This behavior is ideal for websites with static content that doesn't change depending on who views it, or for websites that don't use cookies, headers, or query strings to personalize content.     * __@dont-cache@ __ - This option is best for sites that serve a mix of static and dynamic content. When specified, your distribution caches and serve only the content that is specified in the distribution's @CacheBehaviorPerPath@ parameter. This behavior is ideal for websites or web applications that use cookies, headers, and query strings to personalize content for individual users.
cacheBehavior ::
  CacheBehavior
cacheBehavior = CacheBehavior' {_cbBehavior = Nothing}

-- | The cache behavior of the distribution. The following cache behaviors can be specified:     * __@cache@ __ - This option is best for static sites. When specified, your distribution caches and serves your entire website as static content. This behavior is ideal for websites with static content that doesn't change depending on who views it, or for websites that don't use cookies, headers, or query strings to personalize content.     * __@dont-cache@ __ - This option is best for sites that serve a mix of static and dynamic content. When specified, your distribution caches and serve only the content that is specified in the distribution's @CacheBehaviorPerPath@ parameter. This behavior is ideal for websites or web applications that use cookies, headers, and query strings to personalize content for individual users.
cbBehavior :: Lens' CacheBehavior (Maybe BehaviorEnum)
cbBehavior = lens _cbBehavior (\s a -> s {_cbBehavior = a})

instance FromJSON CacheBehavior where
  parseJSON =
    withObject
      "CacheBehavior"
      (\x -> CacheBehavior' <$> (x .:? "behavior"))

instance Hashable CacheBehavior

instance NFData CacheBehavior

instance ToJSON CacheBehavior where
  toJSON CacheBehavior' {..} =
    object (catMaybes [("behavior" .=) <$> _cbBehavior])
