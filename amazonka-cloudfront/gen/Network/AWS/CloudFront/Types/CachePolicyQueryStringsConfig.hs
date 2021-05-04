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
-- Module      : Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig where

import Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
import Network.AWS.CloudFront.Types.QueryStringNames
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that determines whether any URL query strings in viewer
-- requests (and if so, which query strings) are included in the cache key
-- and automatically included in requests that CloudFront sends to the
-- origin.
--
-- /See:/ 'newCachePolicyQueryStringsConfig' smart constructor.
data CachePolicyQueryStringsConfig = CachePolicyQueryStringsConfig'
  { -- | Contains the specific query strings in viewer requests that either
    -- /__are__/ or /__are not__/ included in the cache key and automatically
    -- included in requests that CloudFront sends to the origin. The behavior
    -- depends on whether the @QueryStringBehavior@ field in the
    -- @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed
    -- query strings /__are__/ included) or @allExcept@ (the listed query
    -- strings /__are not__/ included, but all other query strings are).
    queryStrings :: Prelude.Maybe QueryStringNames,
    -- | Determines whether any URL query strings in viewer requests are included
    -- in the cache key and automatically included in requests that CloudFront
    -- sends to the origin. Valid values are:
    --
    -- -   @none@ – Query strings in viewer requests are not included in the
    --     cache key and are not automatically included in requests that
    --     CloudFront sends to the origin. Even when this field is set to
    --     @none@, any query strings that are listed in an
    --     @OriginRequestPolicy@ /are/ included in origin requests.
    --
    -- -   @whitelist@ – The query strings in viewer requests that are listed
    --     in the @QueryStringNames@ type are included in the cache key and
    --     automatically included in requests that CloudFront sends to the
    --     origin.
    --
    -- -   @allExcept@ – All query strings in viewer requests that are
    --     /__not__/ listed in the @QueryStringNames@ type are included in the
    --     cache key and automatically included in requests that CloudFront
    --     sends to the origin.
    --
    -- -   @all@ – All query strings in viewer requests are included in the
    --     cache key and are automatically included in requests that CloudFront
    --     sends to the origin.
    queryStringBehavior :: CachePolicyQueryStringBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CachePolicyQueryStringsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryStrings', 'cachePolicyQueryStringsConfig_queryStrings' - Contains the specific query strings in viewer requests that either
-- /__are__/ or /__are not__/ included in the cache key and automatically
-- included in requests that CloudFront sends to the origin. The behavior
-- depends on whether the @QueryStringBehavior@ field in the
-- @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed
-- query strings /__are__/ included) or @allExcept@ (the listed query
-- strings /__are not__/ included, but all other query strings are).
--
-- 'queryStringBehavior', 'cachePolicyQueryStringsConfig_queryStringBehavior' - Determines whether any URL query strings in viewer requests are included
-- in the cache key and automatically included in requests that CloudFront
-- sends to the origin. Valid values are:
--
-- -   @none@ – Query strings in viewer requests are not included in the
--     cache key and are not automatically included in requests that
--     CloudFront sends to the origin. Even when this field is set to
--     @none@, any query strings that are listed in an
--     @OriginRequestPolicy@ /are/ included in origin requests.
--
-- -   @whitelist@ – The query strings in viewer requests that are listed
--     in the @QueryStringNames@ type are included in the cache key and
--     automatically included in requests that CloudFront sends to the
--     origin.
--
-- -   @allExcept@ – All query strings in viewer requests that are
--     /__not__/ listed in the @QueryStringNames@ type are included in the
--     cache key and automatically included in requests that CloudFront
--     sends to the origin.
--
-- -   @all@ – All query strings in viewer requests are included in the
--     cache key and are automatically included in requests that CloudFront
--     sends to the origin.
newCachePolicyQueryStringsConfig ::
  -- | 'queryStringBehavior'
  CachePolicyQueryStringBehavior ->
  CachePolicyQueryStringsConfig
newCachePolicyQueryStringsConfig
  pQueryStringBehavior_ =
    CachePolicyQueryStringsConfig'
      { queryStrings =
          Prelude.Nothing,
        queryStringBehavior = pQueryStringBehavior_
      }

-- | Contains the specific query strings in viewer requests that either
-- /__are__/ or /__are not__/ included in the cache key and automatically
-- included in requests that CloudFront sends to the origin. The behavior
-- depends on whether the @QueryStringBehavior@ field in the
-- @CachePolicyQueryStringsConfig@ type is set to @whitelist@ (the listed
-- query strings /__are__/ included) or @allExcept@ (the listed query
-- strings /__are not__/ included, but all other query strings are).
cachePolicyQueryStringsConfig_queryStrings :: Lens.Lens' CachePolicyQueryStringsConfig (Prelude.Maybe QueryStringNames)
cachePolicyQueryStringsConfig_queryStrings = Lens.lens (\CachePolicyQueryStringsConfig' {queryStrings} -> queryStrings) (\s@CachePolicyQueryStringsConfig' {} a -> s {queryStrings = a} :: CachePolicyQueryStringsConfig)

-- | Determines whether any URL query strings in viewer requests are included
-- in the cache key and automatically included in requests that CloudFront
-- sends to the origin. Valid values are:
--
-- -   @none@ – Query strings in viewer requests are not included in the
--     cache key and are not automatically included in requests that
--     CloudFront sends to the origin. Even when this field is set to
--     @none@, any query strings that are listed in an
--     @OriginRequestPolicy@ /are/ included in origin requests.
--
-- -   @whitelist@ – The query strings in viewer requests that are listed
--     in the @QueryStringNames@ type are included in the cache key and
--     automatically included in requests that CloudFront sends to the
--     origin.
--
-- -   @allExcept@ – All query strings in viewer requests that are
--     /__not__/ listed in the @QueryStringNames@ type are included in the
--     cache key and automatically included in requests that CloudFront
--     sends to the origin.
--
-- -   @all@ – All query strings in viewer requests are included in the
--     cache key and are automatically included in requests that CloudFront
--     sends to the origin.
cachePolicyQueryStringsConfig_queryStringBehavior :: Lens.Lens' CachePolicyQueryStringsConfig CachePolicyQueryStringBehavior
cachePolicyQueryStringsConfig_queryStringBehavior = Lens.lens (\CachePolicyQueryStringsConfig' {queryStringBehavior} -> queryStringBehavior) (\s@CachePolicyQueryStringsConfig' {} a -> s {queryStringBehavior = a} :: CachePolicyQueryStringsConfig)

instance
  Prelude.FromXML
    CachePolicyQueryStringsConfig
  where
  parseXML x =
    CachePolicyQueryStringsConfig'
      Prelude.<$> (x Prelude..@? "QueryStrings")
      Prelude.<*> (x Prelude..@ "QueryStringBehavior")

instance
  Prelude.Hashable
    CachePolicyQueryStringsConfig

instance Prelude.NFData CachePolicyQueryStringsConfig

instance Prelude.ToXML CachePolicyQueryStringsConfig where
  toXML CachePolicyQueryStringsConfig' {..} =
    Prelude.mconcat
      [ "QueryStrings" Prelude.@= queryStrings,
        "QueryStringBehavior" Prelude.@= queryStringBehavior
      ]
