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
-- Module      : Amazonka.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginRequestPolicyQueryStringsConfig where

import Amazonka.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
import Amazonka.CloudFront.Types.QueryStringNames
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that determines whether any URL query strings in viewer
-- requests (and if so, which query strings) are included in requests that
-- CloudFront sends to the origin.
--
-- /See:/ 'newOriginRequestPolicyQueryStringsConfig' smart constructor.
data OriginRequestPolicyQueryStringsConfig = OriginRequestPolicyQueryStringsConfig'
  { -- | Contains the specific query strings in viewer requests that either
    -- /__are__/ or /__are not__/ included in requests that CloudFront sends to
    -- the origin. The behavior depends on whether the @QueryStringBehavior@
    -- field in the @OriginRequestPolicyQueryStringsConfig@ type is set to
    -- @whitelist@ (the listed query strings /__are__/ included) or @allExcept@
    -- (the listed query strings /__are not__/ included, but all other query
    -- strings are).
    queryStrings :: Prelude.Maybe QueryStringNames,
    -- | Determines whether any URL query strings in viewer requests are included
    -- in requests that CloudFront sends to the origin. Valid values are:
    --
    -- -   @none@ – No query strings in viewer requests are included in
    --     requests that CloudFront sends to the origin. Even when this field
    --     is set to @none@, any query strings that are listed in a
    --     @CachePolicy@ /are/ included in origin requests.
    --
    -- -   @whitelist@ – Only the query strings in viewer requests that are
    --     listed in the @QueryStringNames@ type are included in requests that
    --     CloudFront sends to the origin.
    --
    -- -   @all@ – All query strings in viewer requests are included in
    --     requests that CloudFront sends to the origin.
    --
    -- -   @allExcept@ – All query strings in viewer requests are included in
    --     requests that CloudFront sends to the origin, /__except__/ for those
    --     listed in the @QueryStringNames@ type, which are not included.
    queryStringBehavior :: OriginRequestPolicyQueryStringBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicyQueryStringsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryStrings', 'originRequestPolicyQueryStringsConfig_queryStrings' - Contains the specific query strings in viewer requests that either
-- /__are__/ or /__are not__/ included in requests that CloudFront sends to
-- the origin. The behavior depends on whether the @QueryStringBehavior@
-- field in the @OriginRequestPolicyQueryStringsConfig@ type is set to
-- @whitelist@ (the listed query strings /__are__/ included) or @allExcept@
-- (the listed query strings /__are not__/ included, but all other query
-- strings are).
--
-- 'queryStringBehavior', 'originRequestPolicyQueryStringsConfig_queryStringBehavior' - Determines whether any URL query strings in viewer requests are included
-- in requests that CloudFront sends to the origin. Valid values are:
--
-- -   @none@ – No query strings in viewer requests are included in
--     requests that CloudFront sends to the origin. Even when this field
--     is set to @none@, any query strings that are listed in a
--     @CachePolicy@ /are/ included in origin requests.
--
-- -   @whitelist@ – Only the query strings in viewer requests that are
--     listed in the @QueryStringNames@ type are included in requests that
--     CloudFront sends to the origin.
--
-- -   @all@ – All query strings in viewer requests are included in
--     requests that CloudFront sends to the origin.
--
-- -   @allExcept@ – All query strings in viewer requests are included in
--     requests that CloudFront sends to the origin, /__except__/ for those
--     listed in the @QueryStringNames@ type, which are not included.
newOriginRequestPolicyQueryStringsConfig ::
  -- | 'queryStringBehavior'
  OriginRequestPolicyQueryStringBehavior ->
  OriginRequestPolicyQueryStringsConfig
newOriginRequestPolicyQueryStringsConfig
  pQueryStringBehavior_ =
    OriginRequestPolicyQueryStringsConfig'
      { queryStrings =
          Prelude.Nothing,
        queryStringBehavior =
          pQueryStringBehavior_
      }

-- | Contains the specific query strings in viewer requests that either
-- /__are__/ or /__are not__/ included in requests that CloudFront sends to
-- the origin. The behavior depends on whether the @QueryStringBehavior@
-- field in the @OriginRequestPolicyQueryStringsConfig@ type is set to
-- @whitelist@ (the listed query strings /__are__/ included) or @allExcept@
-- (the listed query strings /__are not__/ included, but all other query
-- strings are).
originRequestPolicyQueryStringsConfig_queryStrings :: Lens.Lens' OriginRequestPolicyQueryStringsConfig (Prelude.Maybe QueryStringNames)
originRequestPolicyQueryStringsConfig_queryStrings = Lens.lens (\OriginRequestPolicyQueryStringsConfig' {queryStrings} -> queryStrings) (\s@OriginRequestPolicyQueryStringsConfig' {} a -> s {queryStrings = a} :: OriginRequestPolicyQueryStringsConfig)

-- | Determines whether any URL query strings in viewer requests are included
-- in requests that CloudFront sends to the origin. Valid values are:
--
-- -   @none@ – No query strings in viewer requests are included in
--     requests that CloudFront sends to the origin. Even when this field
--     is set to @none@, any query strings that are listed in a
--     @CachePolicy@ /are/ included in origin requests.
--
-- -   @whitelist@ – Only the query strings in viewer requests that are
--     listed in the @QueryStringNames@ type are included in requests that
--     CloudFront sends to the origin.
--
-- -   @all@ – All query strings in viewer requests are included in
--     requests that CloudFront sends to the origin.
--
-- -   @allExcept@ – All query strings in viewer requests are included in
--     requests that CloudFront sends to the origin, /__except__/ for those
--     listed in the @QueryStringNames@ type, which are not included.
originRequestPolicyQueryStringsConfig_queryStringBehavior :: Lens.Lens' OriginRequestPolicyQueryStringsConfig OriginRequestPolicyQueryStringBehavior
originRequestPolicyQueryStringsConfig_queryStringBehavior = Lens.lens (\OriginRequestPolicyQueryStringsConfig' {queryStringBehavior} -> queryStringBehavior) (\s@OriginRequestPolicyQueryStringsConfig' {} a -> s {queryStringBehavior = a} :: OriginRequestPolicyQueryStringsConfig)

instance
  Data.FromXML
    OriginRequestPolicyQueryStringsConfig
  where
  parseXML x =
    OriginRequestPolicyQueryStringsConfig'
      Prelude.<$> (x Data..@? "QueryStrings")
      Prelude.<*> (x Data..@ "QueryStringBehavior")

instance
  Prelude.Hashable
    OriginRequestPolicyQueryStringsConfig
  where
  hashWithSalt
    _salt
    OriginRequestPolicyQueryStringsConfig' {..} =
      _salt
        `Prelude.hashWithSalt` queryStrings
        `Prelude.hashWithSalt` queryStringBehavior

instance
  Prelude.NFData
    OriginRequestPolicyQueryStringsConfig
  where
  rnf OriginRequestPolicyQueryStringsConfig' {..} =
    Prelude.rnf queryStrings
      `Prelude.seq` Prelude.rnf queryStringBehavior

instance
  Data.ToXML
    OriginRequestPolicyQueryStringsConfig
  where
  toXML OriginRequestPolicyQueryStringsConfig' {..} =
    Prelude.mconcat
      [ "QueryStrings" Data.@= queryStrings,
        "QueryStringBehavior" Data.@= queryStringBehavior
      ]
