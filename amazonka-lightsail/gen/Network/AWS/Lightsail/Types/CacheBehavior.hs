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
-- Module      : Network.AWS.Lightsail.Types.CacheBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CacheBehavior where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.BehaviorEnum
import qualified Network.AWS.Prelude as Prelude

-- | Describes the default cache behavior of an Amazon Lightsail content
-- delivery network (CDN) distribution.
--
-- /See:/ 'newCacheBehavior' smart constructor.
data CacheBehavior = CacheBehavior'
  { -- | The cache behavior of the distribution.
    --
    -- The following cache behaviors can be specified:
    --
    -- -   __@cache@__ - This option is best for static sites. When specified,
    --     your distribution caches and serves your entire website as static
    --     content. This behavior is ideal for websites with static content
    --     that doesn\'t change depending on who views it, or for websites that
    --     don\'t use cookies, headers, or query strings to personalize
    --     content.
    --
    -- -   __@dont-cache@__ - This option is best for sites that serve a mix of
    --     static and dynamic content. When specified, your distribution caches
    --     and serve only the content that is specified in the distribution\'s
    --     @CacheBehaviorPerPath@ parameter. This behavior is ideal for
    --     websites or web applications that use cookies, headers, and query
    --     strings to personalize content for individual users.
    behavior :: Prelude.Maybe BehaviorEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CacheBehavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'behavior', 'cacheBehavior_behavior' - The cache behavior of the distribution.
--
-- The following cache behaviors can be specified:
--
-- -   __@cache@__ - This option is best for static sites. When specified,
--     your distribution caches and serves your entire website as static
--     content. This behavior is ideal for websites with static content
--     that doesn\'t change depending on who views it, or for websites that
--     don\'t use cookies, headers, or query strings to personalize
--     content.
--
-- -   __@dont-cache@__ - This option is best for sites that serve a mix of
--     static and dynamic content. When specified, your distribution caches
--     and serve only the content that is specified in the distribution\'s
--     @CacheBehaviorPerPath@ parameter. This behavior is ideal for
--     websites or web applications that use cookies, headers, and query
--     strings to personalize content for individual users.
newCacheBehavior ::
  CacheBehavior
newCacheBehavior =
  CacheBehavior' {behavior = Prelude.Nothing}

-- | The cache behavior of the distribution.
--
-- The following cache behaviors can be specified:
--
-- -   __@cache@__ - This option is best for static sites. When specified,
--     your distribution caches and serves your entire website as static
--     content. This behavior is ideal for websites with static content
--     that doesn\'t change depending on who views it, or for websites that
--     don\'t use cookies, headers, or query strings to personalize
--     content.
--
-- -   __@dont-cache@__ - This option is best for sites that serve a mix of
--     static and dynamic content. When specified, your distribution caches
--     and serve only the content that is specified in the distribution\'s
--     @CacheBehaviorPerPath@ parameter. This behavior is ideal for
--     websites or web applications that use cookies, headers, and query
--     strings to personalize content for individual users.
cacheBehavior_behavior :: Lens.Lens' CacheBehavior (Prelude.Maybe BehaviorEnum)
cacheBehavior_behavior = Lens.lens (\CacheBehavior' {behavior} -> behavior) (\s@CacheBehavior' {} a -> s {behavior = a} :: CacheBehavior)

instance Prelude.FromJSON CacheBehavior where
  parseJSON =
    Prelude.withObject
      "CacheBehavior"
      ( \x ->
          CacheBehavior'
            Prelude.<$> (x Prelude..:? "behavior")
      )

instance Prelude.Hashable CacheBehavior

instance Prelude.NFData CacheBehavior

instance Prelude.ToJSON CacheBehavior where
  toJSON CacheBehavior' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("behavior" Prelude..=) Prelude.<$> behavior]
      )
