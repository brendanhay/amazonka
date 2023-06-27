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
-- Module      : Amazonka.CloudFront.Types.OriginShield
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginShield where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | CloudFront Origin Shield.
--
-- Using Origin Shield can help reduce the load on your origin. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newOriginShield' smart constructor.
data OriginShield = OriginShield'
  { -- | The Amazon Web Services Region for Origin Shield.
    --
    -- Specify the Amazon Web Services Region that has the lowest latency to
    -- your origin. To specify a region, use the region code, not the region
    -- name. For example, specify the US East (Ohio) region as @us-east-2@.
    --
    -- When you enable CloudFront Origin Shield, you must specify the Amazon
    -- Web Services Region for Origin Shield. For the list of Amazon Web
    -- Services Regions that you can specify, and for help choosing the best
    -- Region for your origin, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the Amazon Web Services Region for Origin Shield>
    -- in the /Amazon CloudFront Developer Guide/.
    originShieldRegion :: Prelude.Maybe Prelude.Text,
    -- | A flag that specifies whether Origin Shield is enabled.
    --
    -- When it\'s enabled, CloudFront routes all requests through Origin
    -- Shield, which can help protect your origin. When it\'s disabled,
    -- CloudFront might send requests directly to your origin from multiple
    -- edge locations or regional edge caches.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginShield' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originShieldRegion', 'originShield_originShieldRegion' - The Amazon Web Services Region for Origin Shield.
--
-- Specify the Amazon Web Services Region that has the lowest latency to
-- your origin. To specify a region, use the region code, not the region
-- name. For example, specify the US East (Ohio) region as @us-east-2@.
--
-- When you enable CloudFront Origin Shield, you must specify the Amazon
-- Web Services Region for Origin Shield. For the list of Amazon Web
-- Services Regions that you can specify, and for help choosing the best
-- Region for your origin, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the Amazon Web Services Region for Origin Shield>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'enabled', 'originShield_enabled' - A flag that specifies whether Origin Shield is enabled.
--
-- When it\'s enabled, CloudFront routes all requests through Origin
-- Shield, which can help protect your origin. When it\'s disabled,
-- CloudFront might send requests directly to your origin from multiple
-- edge locations or regional edge caches.
newOriginShield ::
  -- | 'enabled'
  Prelude.Bool ->
  OriginShield
newOriginShield pEnabled_ =
  OriginShield'
    { originShieldRegion = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The Amazon Web Services Region for Origin Shield.
--
-- Specify the Amazon Web Services Region that has the lowest latency to
-- your origin. To specify a region, use the region code, not the region
-- name. For example, specify the US East (Ohio) region as @us-east-2@.
--
-- When you enable CloudFront Origin Shield, you must specify the Amazon
-- Web Services Region for Origin Shield. For the list of Amazon Web
-- Services Regions that you can specify, and for help choosing the best
-- Region for your origin, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the Amazon Web Services Region for Origin Shield>
-- in the /Amazon CloudFront Developer Guide/.
originShield_originShieldRegion :: Lens.Lens' OriginShield (Prelude.Maybe Prelude.Text)
originShield_originShieldRegion = Lens.lens (\OriginShield' {originShieldRegion} -> originShieldRegion) (\s@OriginShield' {} a -> s {originShieldRegion = a} :: OriginShield)

-- | A flag that specifies whether Origin Shield is enabled.
--
-- When it\'s enabled, CloudFront routes all requests through Origin
-- Shield, which can help protect your origin. When it\'s disabled,
-- CloudFront might send requests directly to your origin from multiple
-- edge locations or regional edge caches.
originShield_enabled :: Lens.Lens' OriginShield Prelude.Bool
originShield_enabled = Lens.lens (\OriginShield' {enabled} -> enabled) (\s@OriginShield' {} a -> s {enabled = a} :: OriginShield)

instance Data.FromXML OriginShield where
  parseXML x =
    OriginShield'
      Prelude.<$> (x Data..@? "OriginShieldRegion")
      Prelude.<*> (x Data..@ "Enabled")

instance Prelude.Hashable OriginShield where
  hashWithSalt _salt OriginShield' {..} =
    _salt
      `Prelude.hashWithSalt` originShieldRegion
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData OriginShield where
  rnf OriginShield' {..} =
    Prelude.rnf originShieldRegion
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToXML OriginShield where
  toXML OriginShield' {..} =
    Prelude.mconcat
      [ "OriginShieldRegion" Data.@= originShieldRegion,
        "Enabled" Data.@= enabled
      ]
