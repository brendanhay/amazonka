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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDefaultCacheBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDefaultCacheBehavior where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the default cache configuration for the
-- CloudFront distribution.
--
-- /See:/ 'newAwsCloudFrontDistributionDefaultCacheBehavior' smart constructor.
data AwsCloudFrontDistributionDefaultCacheBehavior = AwsCloudFrontDistributionDefaultCacheBehavior'
  { -- | The protocol that viewers can use to access the files in an origin. You
    -- can specify the following options:
    --
    -- -   @allow-all@ - Viewers can use HTTP or HTTPS.
    --
    -- -   @redirect-to-https@ - CloudFront responds to HTTP requests with an
    --     HTTP status code of 301 (Moved Permanently) and the HTTPS URL. The
    --     viewer then uses the new URL to resubmit.
    --
    -- -   @https-only@ - CloudFront responds to HTTP request with an HTTP
    --     status code of 403 (Forbidden).
    viewerProtocolPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionDefaultCacheBehavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewerProtocolPolicy', 'awsCloudFrontDistributionDefaultCacheBehavior_viewerProtocolPolicy' - The protocol that viewers can use to access the files in an origin. You
-- can specify the following options:
--
-- -   @allow-all@ - Viewers can use HTTP or HTTPS.
--
-- -   @redirect-to-https@ - CloudFront responds to HTTP requests with an
--     HTTP status code of 301 (Moved Permanently) and the HTTPS URL. The
--     viewer then uses the new URL to resubmit.
--
-- -   @https-only@ - CloudFront responds to HTTP request with an HTTP
--     status code of 403 (Forbidden).
newAwsCloudFrontDistributionDefaultCacheBehavior ::
  AwsCloudFrontDistributionDefaultCacheBehavior
newAwsCloudFrontDistributionDefaultCacheBehavior =
  AwsCloudFrontDistributionDefaultCacheBehavior'
    { viewerProtocolPolicy =
        Prelude.Nothing
    }

-- | The protocol that viewers can use to access the files in an origin. You
-- can specify the following options:
--
-- -   @allow-all@ - Viewers can use HTTP or HTTPS.
--
-- -   @redirect-to-https@ - CloudFront responds to HTTP requests with an
--     HTTP status code of 301 (Moved Permanently) and the HTTPS URL. The
--     viewer then uses the new URL to resubmit.
--
-- -   @https-only@ - CloudFront responds to HTTP request with an HTTP
--     status code of 403 (Forbidden).
awsCloudFrontDistributionDefaultCacheBehavior_viewerProtocolPolicy :: Lens.Lens' AwsCloudFrontDistributionDefaultCacheBehavior (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDefaultCacheBehavior_viewerProtocolPolicy = Lens.lens (\AwsCloudFrontDistributionDefaultCacheBehavior' {viewerProtocolPolicy} -> viewerProtocolPolicy) (\s@AwsCloudFrontDistributionDefaultCacheBehavior' {} a -> s {viewerProtocolPolicy = a} :: AwsCloudFrontDistributionDefaultCacheBehavior)

instance
  Core.FromJSON
    AwsCloudFrontDistributionDefaultCacheBehavior
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionDefaultCacheBehavior"
      ( \x ->
          AwsCloudFrontDistributionDefaultCacheBehavior'
            Prelude.<$> (x Core..:? "ViewerProtocolPolicy")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionDefaultCacheBehavior
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionDefaultCacheBehavior' {..} =
      _salt `Prelude.hashWithSalt` viewerProtocolPolicy

instance
  Prelude.NFData
    AwsCloudFrontDistributionDefaultCacheBehavior
  where
  rnf
    AwsCloudFrontDistributionDefaultCacheBehavior' {..} =
      Prelude.rnf viewerProtocolPolicy

instance
  Core.ToJSON
    AwsCloudFrontDistributionDefaultCacheBehavior
  where
  toJSON
    AwsCloudFrontDistributionDefaultCacheBehavior' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ViewerProtocolPolicy" Core..=)
                Prelude.<$> viewerProtocolPolicy
            ]
        )
