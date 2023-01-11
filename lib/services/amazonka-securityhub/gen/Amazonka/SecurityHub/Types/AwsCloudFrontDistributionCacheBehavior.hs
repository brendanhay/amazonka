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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehavior where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a cache behavior for the distribution.
--
-- /See:/ 'newAwsCloudFrontDistributionCacheBehavior' smart constructor.
data AwsCloudFrontDistributionCacheBehavior = AwsCloudFrontDistributionCacheBehavior'
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
-- Create a value of 'AwsCloudFrontDistributionCacheBehavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewerProtocolPolicy', 'awsCloudFrontDistributionCacheBehavior_viewerProtocolPolicy' - The protocol that viewers can use to access the files in an origin. You
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
newAwsCloudFrontDistributionCacheBehavior ::
  AwsCloudFrontDistributionCacheBehavior
newAwsCloudFrontDistributionCacheBehavior =
  AwsCloudFrontDistributionCacheBehavior'
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
awsCloudFrontDistributionCacheBehavior_viewerProtocolPolicy :: Lens.Lens' AwsCloudFrontDistributionCacheBehavior (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionCacheBehavior_viewerProtocolPolicy = Lens.lens (\AwsCloudFrontDistributionCacheBehavior' {viewerProtocolPolicy} -> viewerProtocolPolicy) (\s@AwsCloudFrontDistributionCacheBehavior' {} a -> s {viewerProtocolPolicy = a} :: AwsCloudFrontDistributionCacheBehavior)

instance
  Data.FromJSON
    AwsCloudFrontDistributionCacheBehavior
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionCacheBehavior"
      ( \x ->
          AwsCloudFrontDistributionCacheBehavior'
            Prelude.<$> (x Data..:? "ViewerProtocolPolicy")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionCacheBehavior
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionCacheBehavior' {..} =
      _salt `Prelude.hashWithSalt` viewerProtocolPolicy

instance
  Prelude.NFData
    AwsCloudFrontDistributionCacheBehavior
  where
  rnf AwsCloudFrontDistributionCacheBehavior' {..} =
    Prelude.rnf viewerProtocolPolicy

instance
  Data.ToJSON
    AwsCloudFrontDistributionCacheBehavior
  where
  toJSON AwsCloudFrontDistributionCacheBehavior' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ViewerProtocolPolicy" Data..=)
              Prelude.<$> viewerProtocolPolicy
          ]
      )
