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
-- Module      : Amazonka.CloudFront.Types.OriginRequestPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginRequestPolicy where

import Amazonka.CloudFront.Types.OriginRequestPolicyConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An origin request policy.
--
-- When it’s attached to a cache behavior, the origin request policy
-- determines the values that CloudFront includes in requests that it sends
-- to the origin. Each request that CloudFront sends to the origin includes
-- the following:
--
-- -   The request body and the URL path (without the domain name) from the
--     viewer request.
--
-- -   The headers that CloudFront automatically includes in every origin
--     request, including @Host@, @User-Agent@, and @X-Amz-Cf-Id@.
--
-- -   All HTTP headers, cookies, and URL query strings that are specified
--     in the cache policy or the origin request policy. These can include
--     items from the viewer request and, in the case of headers,
--     additional ones that are added by CloudFront.
--
-- CloudFront sends a request when it can’t find an object in its cache
-- that matches the request. If you want to send values to the origin and
-- also include them in the cache key, use @CachePolicy@.
--
-- /See:/ 'newOriginRequestPolicy' smart constructor.
data OriginRequestPolicy = OriginRequestPolicy'
  { -- | The unique identifier for the origin request policy.
    id :: Prelude.Text,
    -- | The date and time when the origin request policy was last modified.
    lastModifiedTime :: Core.ISO8601,
    -- | The origin request policy configuration.
    originRequestPolicyConfig :: OriginRequestPolicyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'originRequestPolicy_id' - The unique identifier for the origin request policy.
--
-- 'lastModifiedTime', 'originRequestPolicy_lastModifiedTime' - The date and time when the origin request policy was last modified.
--
-- 'originRequestPolicyConfig', 'originRequestPolicy_originRequestPolicyConfig' - The origin request policy configuration.
newOriginRequestPolicy ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'originRequestPolicyConfig'
  OriginRequestPolicyConfig ->
  OriginRequestPolicy
newOriginRequestPolicy
  pId_
  pLastModifiedTime_
  pOriginRequestPolicyConfig_ =
    OriginRequestPolicy'
      { id = pId_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        originRequestPolicyConfig =
          pOriginRequestPolicyConfig_
      }

-- | The unique identifier for the origin request policy.
originRequestPolicy_id :: Lens.Lens' OriginRequestPolicy Prelude.Text
originRequestPolicy_id = Lens.lens (\OriginRequestPolicy' {id} -> id) (\s@OriginRequestPolicy' {} a -> s {id = a} :: OriginRequestPolicy)

-- | The date and time when the origin request policy was last modified.
originRequestPolicy_lastModifiedTime :: Lens.Lens' OriginRequestPolicy Prelude.UTCTime
originRequestPolicy_lastModifiedTime = Lens.lens (\OriginRequestPolicy' {lastModifiedTime} -> lastModifiedTime) (\s@OriginRequestPolicy' {} a -> s {lastModifiedTime = a} :: OriginRequestPolicy) Prelude.. Core._Time

-- | The origin request policy configuration.
originRequestPolicy_originRequestPolicyConfig :: Lens.Lens' OriginRequestPolicy OriginRequestPolicyConfig
originRequestPolicy_originRequestPolicyConfig = Lens.lens (\OriginRequestPolicy' {originRequestPolicyConfig} -> originRequestPolicyConfig) (\s@OriginRequestPolicy' {} a -> s {originRequestPolicyConfig = a} :: OriginRequestPolicy)

instance Core.FromXML OriginRequestPolicy where
  parseXML x =
    OriginRequestPolicy'
      Prelude.<$> (x Core..@ "Id")
      Prelude.<*> (x Core..@ "LastModifiedTime")
      Prelude.<*> (x Core..@ "OriginRequestPolicyConfig")

instance Prelude.Hashable OriginRequestPolicy where
  hashWithSalt _salt OriginRequestPolicy' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` originRequestPolicyConfig

instance Prelude.NFData OriginRequestPolicy where
  rnf OriginRequestPolicy' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf originRequestPolicyConfig
