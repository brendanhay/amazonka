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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig where

import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that determines whether any HTTP headers (and if so, which
-- headers) are included in requests that CloudFront sends to the origin.
--
-- /See:/ 'newOriginRequestPolicyHeadersConfig' smart constructor.
data OriginRequestPolicyHeadersConfig = OriginRequestPolicyHeadersConfig'
  { headers :: Prelude.Maybe Headers,
    -- | Determines whether any HTTP headers are included in requests that
    -- CloudFront sends to the origin. Valid values are:
    --
    -- -   @none@ – HTTP headers are not included in requests that CloudFront
    --     sends to the origin. Even when this field is set to @none@, any
    --     headers that are listed in a @CachePolicy@ /are/ included in origin
    --     requests.
    --
    -- -   @whitelist@ – The HTTP headers that are listed in the @Headers@ type
    --     are included in requests that CloudFront sends to the origin.
    --
    -- -   @allViewer@ – All HTTP headers in viewer requests are included in
    --     requests that CloudFront sends to the origin.
    --
    -- -   @allViewerAndWhitelistCloudFront@ – All HTTP headers in viewer
    --     requests and the additional CloudFront headers that are listed in
    --     the @Headers@ type are included in requests that CloudFront sends to
    --     the origin. The additional headers are added by CloudFront.
    headerBehavior :: OriginRequestPolicyHeaderBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicyHeadersConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'originRequestPolicyHeadersConfig_headers' - Undocumented member.
--
-- 'headerBehavior', 'originRequestPolicyHeadersConfig_headerBehavior' - Determines whether any HTTP headers are included in requests that
-- CloudFront sends to the origin. Valid values are:
--
-- -   @none@ – HTTP headers are not included in requests that CloudFront
--     sends to the origin. Even when this field is set to @none@, any
--     headers that are listed in a @CachePolicy@ /are/ included in origin
--     requests.
--
-- -   @whitelist@ – The HTTP headers that are listed in the @Headers@ type
--     are included in requests that CloudFront sends to the origin.
--
-- -   @allViewer@ – All HTTP headers in viewer requests are included in
--     requests that CloudFront sends to the origin.
--
-- -   @allViewerAndWhitelistCloudFront@ – All HTTP headers in viewer
--     requests and the additional CloudFront headers that are listed in
--     the @Headers@ type are included in requests that CloudFront sends to
--     the origin. The additional headers are added by CloudFront.
newOriginRequestPolicyHeadersConfig ::
  -- | 'headerBehavior'
  OriginRequestPolicyHeaderBehavior ->
  OriginRequestPolicyHeadersConfig
newOriginRequestPolicyHeadersConfig pHeaderBehavior_ =
  OriginRequestPolicyHeadersConfig'
    { headers =
        Prelude.Nothing,
      headerBehavior = pHeaderBehavior_
    }

-- | Undocumented member.
originRequestPolicyHeadersConfig_headers :: Lens.Lens' OriginRequestPolicyHeadersConfig (Prelude.Maybe Headers)
originRequestPolicyHeadersConfig_headers = Lens.lens (\OriginRequestPolicyHeadersConfig' {headers} -> headers) (\s@OriginRequestPolicyHeadersConfig' {} a -> s {headers = a} :: OriginRequestPolicyHeadersConfig)

-- | Determines whether any HTTP headers are included in requests that
-- CloudFront sends to the origin. Valid values are:
--
-- -   @none@ – HTTP headers are not included in requests that CloudFront
--     sends to the origin. Even when this field is set to @none@, any
--     headers that are listed in a @CachePolicy@ /are/ included in origin
--     requests.
--
-- -   @whitelist@ – The HTTP headers that are listed in the @Headers@ type
--     are included in requests that CloudFront sends to the origin.
--
-- -   @allViewer@ – All HTTP headers in viewer requests are included in
--     requests that CloudFront sends to the origin.
--
-- -   @allViewerAndWhitelistCloudFront@ – All HTTP headers in viewer
--     requests and the additional CloudFront headers that are listed in
--     the @Headers@ type are included in requests that CloudFront sends to
--     the origin. The additional headers are added by CloudFront.
originRequestPolicyHeadersConfig_headerBehavior :: Lens.Lens' OriginRequestPolicyHeadersConfig OriginRequestPolicyHeaderBehavior
originRequestPolicyHeadersConfig_headerBehavior = Lens.lens (\OriginRequestPolicyHeadersConfig' {headerBehavior} -> headerBehavior) (\s@OriginRequestPolicyHeadersConfig' {} a -> s {headerBehavior = a} :: OriginRequestPolicyHeadersConfig)

instance
  Prelude.FromXML
    OriginRequestPolicyHeadersConfig
  where
  parseXML x =
    OriginRequestPolicyHeadersConfig'
      Prelude.<$> (x Prelude..@? "Headers")
      Prelude.<*> (x Prelude..@ "HeaderBehavior")

instance
  Prelude.Hashable
    OriginRequestPolicyHeadersConfig

instance
  Prelude.NFData
    OriginRequestPolicyHeadersConfig

instance
  Prelude.ToXML
    OriginRequestPolicyHeadersConfig
  where
  toXML OriginRequestPolicyHeadersConfig' {..} =
    Prelude.mconcat
      [ "Headers" Prelude.@= headers,
        "HeaderBehavior" Prelude.@= headerBehavior
      ]
