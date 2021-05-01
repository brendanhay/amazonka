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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig where

import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
import Network.AWS.CloudFront.Types.QueryStringNames
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that determines whether any URL query strings in viewer
-- requests (and if so, which query strings) are included in requests that
-- CloudFront sends to the origin.
--
-- /See:/ 'newOriginRequestPolicyQueryStringsConfig' smart constructor.
data OriginRequestPolicyQueryStringsConfig = OriginRequestPolicyQueryStringsConfig'
  { -- | Contains a list of the query strings in viewer requests that are
    -- included in requests that CloudFront sends to the origin.
    queryStrings :: Prelude.Maybe QueryStringNames,
    -- | Determines whether any URL query strings in viewer requests are included
    -- in requests that CloudFront sends to the origin. Valid values are:
    --
    -- -   @none@ – Query strings in viewer requests are not included in
    --     requests that CloudFront sends to the origin. Even when this field
    --     is set to @none@, any query strings that are listed in a
    --     @CachePolicy@ /are/ included in origin requests.
    --
    -- -   @whitelist@ – The query strings in viewer requests that are listed
    --     in the @QueryStringNames@ type are included in requests that
    --     CloudFront sends to the origin.
    --
    -- -   @all@ – All query strings in viewer requests are included in
    --     requests that CloudFront sends to the origin.
    queryStringBehavior :: OriginRequestPolicyQueryStringBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicyQueryStringsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryStrings', 'originRequestPolicyQueryStringsConfig_queryStrings' - Contains a list of the query strings in viewer requests that are
-- included in requests that CloudFront sends to the origin.
--
-- 'queryStringBehavior', 'originRequestPolicyQueryStringsConfig_queryStringBehavior' - Determines whether any URL query strings in viewer requests are included
-- in requests that CloudFront sends to the origin. Valid values are:
--
-- -   @none@ – Query strings in viewer requests are not included in
--     requests that CloudFront sends to the origin. Even when this field
--     is set to @none@, any query strings that are listed in a
--     @CachePolicy@ /are/ included in origin requests.
--
-- -   @whitelist@ – The query strings in viewer requests that are listed
--     in the @QueryStringNames@ type are included in requests that
--     CloudFront sends to the origin.
--
-- -   @all@ – All query strings in viewer requests are included in
--     requests that CloudFront sends to the origin.
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

-- | Contains a list of the query strings in viewer requests that are
-- included in requests that CloudFront sends to the origin.
originRequestPolicyQueryStringsConfig_queryStrings :: Lens.Lens' OriginRequestPolicyQueryStringsConfig (Prelude.Maybe QueryStringNames)
originRequestPolicyQueryStringsConfig_queryStrings = Lens.lens (\OriginRequestPolicyQueryStringsConfig' {queryStrings} -> queryStrings) (\s@OriginRequestPolicyQueryStringsConfig' {} a -> s {queryStrings = a} :: OriginRequestPolicyQueryStringsConfig)

-- | Determines whether any URL query strings in viewer requests are included
-- in requests that CloudFront sends to the origin. Valid values are:
--
-- -   @none@ – Query strings in viewer requests are not included in
--     requests that CloudFront sends to the origin. Even when this field
--     is set to @none@, any query strings that are listed in a
--     @CachePolicy@ /are/ included in origin requests.
--
-- -   @whitelist@ – The query strings in viewer requests that are listed
--     in the @QueryStringNames@ type are included in requests that
--     CloudFront sends to the origin.
--
-- -   @all@ – All query strings in viewer requests are included in
--     requests that CloudFront sends to the origin.
originRequestPolicyQueryStringsConfig_queryStringBehavior :: Lens.Lens' OriginRequestPolicyQueryStringsConfig OriginRequestPolicyQueryStringBehavior
originRequestPolicyQueryStringsConfig_queryStringBehavior = Lens.lens (\OriginRequestPolicyQueryStringsConfig' {queryStringBehavior} -> queryStringBehavior) (\s@OriginRequestPolicyQueryStringsConfig' {} a -> s {queryStringBehavior = a} :: OriginRequestPolicyQueryStringsConfig)

instance
  Prelude.FromXML
    OriginRequestPolicyQueryStringsConfig
  where
  parseXML x =
    OriginRequestPolicyQueryStringsConfig'
      Prelude.<$> (x Prelude..@? "QueryStrings")
      Prelude.<*> (x Prelude..@ "QueryStringBehavior")

instance
  Prelude.Hashable
    OriginRequestPolicyQueryStringsConfig

instance
  Prelude.NFData
    OriginRequestPolicyQueryStringsConfig

instance
  Prelude.ToXML
    OriginRequestPolicyQueryStringsConfig
  where
  toXML OriginRequestPolicyQueryStringsConfig' {..} =
    Prelude.mconcat
      [ "QueryStrings" Prelude.@= queryStrings,
        "QueryStringBehavior" Prelude.@= queryStringBehavior
      ]
