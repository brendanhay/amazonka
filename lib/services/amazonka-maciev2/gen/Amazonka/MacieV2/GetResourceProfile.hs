{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.GetResourceProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) sensitive data discovery statistics and the
-- sensitivity score for an S3 bucket.
module Amazonka.MacieV2.GetResourceProfile
  ( -- * Creating a Request
    GetResourceProfile (..),
    newGetResourceProfile,

    -- * Request Lenses
    getResourceProfile_resourceArn,

    -- * Destructuring the Response
    GetResourceProfileResponse (..),
    newGetResourceProfileResponse,

    -- * Response Lenses
    getResourceProfileResponse_profileUpdatedAt,
    getResourceProfileResponse_sensitivityScore,
    getResourceProfileResponse_sensitivityScoreOverridden,
    getResourceProfileResponse_statistics,
    getResourceProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceProfile' smart constructor.
data GetResourceProfile = GetResourceProfile'
  { -- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
    -- to.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getResourceProfile_resourceArn' - The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
newGetResourceProfile ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetResourceProfile
newGetResourceProfile pResourceArn_ =
  GetResourceProfile' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
getResourceProfile_resourceArn :: Lens.Lens' GetResourceProfile Prelude.Text
getResourceProfile_resourceArn = Lens.lens (\GetResourceProfile' {resourceArn} -> resourceArn) (\s@GetResourceProfile' {} a -> s {resourceArn = a} :: GetResourceProfile)

instance Core.AWSRequest GetResourceProfile where
  type
    AWSResponse GetResourceProfile =
      GetResourceProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceProfileResponse'
            Prelude.<$> (x Data..?> "profileUpdatedAt")
            Prelude.<*> (x Data..?> "sensitivityScore")
            Prelude.<*> (x Data..?> "sensitivityScoreOverridden")
            Prelude.<*> (x Data..?> "statistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceProfile where
  hashWithSalt _salt GetResourceProfile' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetResourceProfile where
  rnf GetResourceProfile' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders GetResourceProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetResourceProfile where
  toPath = Prelude.const "/resource-profiles"

instance Data.ToQuery GetResourceProfile where
  toQuery GetResourceProfile' {..} =
    Prelude.mconcat ["resourceArn" Data.=: resourceArn]

-- | /See:/ 'newGetResourceProfileResponse' smart constructor.
data GetResourceProfileResponse = GetResourceProfileResponse'
  { -- | The date and time, in UTC and extended ISO 8601 format, when Amazon
    -- Macie most recently recalculated sensitive data discovery statistics and
    -- details for the bucket. If the bucket\'s sensitivity score is calculated
    -- automatically, this includes the score.
    profileUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The current sensitivity score for the bucket, ranging from -1 (no
    -- analysis due to an error) to 100 (sensitive). By default, this score is
    -- calculated automatically based on the amount of data that Amazon Macie
    -- has analyzed in the bucket and the amount of sensitive data that Macie
    -- has found in the bucket.
    sensitivityScore :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the bucket\'s current sensitivity score was set
    -- manually. If this value is true, the score was manually changed to 100.
    -- If this value is false, the score was calculated automatically by Amazon
    -- Macie.
    sensitivityScoreOverridden :: Prelude.Maybe Prelude.Bool,
    -- | The sensitive data discovery statistics for the bucket. The statistics
    -- capture the results of automated sensitive data discovery activities
    -- that Amazon Macie has performed for the bucket.
    statistics :: Prelude.Maybe ResourceStatistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileUpdatedAt', 'getResourceProfileResponse_profileUpdatedAt' - The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently recalculated sensitive data discovery statistics and
-- details for the bucket. If the bucket\'s sensitivity score is calculated
-- automatically, this includes the score.
--
-- 'sensitivityScore', 'getResourceProfileResponse_sensitivityScore' - The current sensitivity score for the bucket, ranging from -1 (no
-- analysis due to an error) to 100 (sensitive). By default, this score is
-- calculated automatically based on the amount of data that Amazon Macie
-- has analyzed in the bucket and the amount of sensitive data that Macie
-- has found in the bucket.
--
-- 'sensitivityScoreOverridden', 'getResourceProfileResponse_sensitivityScoreOverridden' - Specifies whether the bucket\'s current sensitivity score was set
-- manually. If this value is true, the score was manually changed to 100.
-- If this value is false, the score was calculated automatically by Amazon
-- Macie.
--
-- 'statistics', 'getResourceProfileResponse_statistics' - The sensitive data discovery statistics for the bucket. The statistics
-- capture the results of automated sensitive data discovery activities
-- that Amazon Macie has performed for the bucket.
--
-- 'httpStatus', 'getResourceProfileResponse_httpStatus' - The response's http status code.
newGetResourceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceProfileResponse
newGetResourceProfileResponse pHttpStatus_ =
  GetResourceProfileResponse'
    { profileUpdatedAt =
        Prelude.Nothing,
      sensitivityScore = Prelude.Nothing,
      sensitivityScoreOverridden = Prelude.Nothing,
      statistics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently recalculated sensitive data discovery statistics and
-- details for the bucket. If the bucket\'s sensitivity score is calculated
-- automatically, this includes the score.
getResourceProfileResponse_profileUpdatedAt :: Lens.Lens' GetResourceProfileResponse (Prelude.Maybe Prelude.UTCTime)
getResourceProfileResponse_profileUpdatedAt = Lens.lens (\GetResourceProfileResponse' {profileUpdatedAt} -> profileUpdatedAt) (\s@GetResourceProfileResponse' {} a -> s {profileUpdatedAt = a} :: GetResourceProfileResponse) Prelude.. Lens.mapping Data._Time

-- | The current sensitivity score for the bucket, ranging from -1 (no
-- analysis due to an error) to 100 (sensitive). By default, this score is
-- calculated automatically based on the amount of data that Amazon Macie
-- has analyzed in the bucket and the amount of sensitive data that Macie
-- has found in the bucket.
getResourceProfileResponse_sensitivityScore :: Lens.Lens' GetResourceProfileResponse (Prelude.Maybe Prelude.Int)
getResourceProfileResponse_sensitivityScore = Lens.lens (\GetResourceProfileResponse' {sensitivityScore} -> sensitivityScore) (\s@GetResourceProfileResponse' {} a -> s {sensitivityScore = a} :: GetResourceProfileResponse)

-- | Specifies whether the bucket\'s current sensitivity score was set
-- manually. If this value is true, the score was manually changed to 100.
-- If this value is false, the score was calculated automatically by Amazon
-- Macie.
getResourceProfileResponse_sensitivityScoreOverridden :: Lens.Lens' GetResourceProfileResponse (Prelude.Maybe Prelude.Bool)
getResourceProfileResponse_sensitivityScoreOverridden = Lens.lens (\GetResourceProfileResponse' {sensitivityScoreOverridden} -> sensitivityScoreOverridden) (\s@GetResourceProfileResponse' {} a -> s {sensitivityScoreOverridden = a} :: GetResourceProfileResponse)

-- | The sensitive data discovery statistics for the bucket. The statistics
-- capture the results of automated sensitive data discovery activities
-- that Amazon Macie has performed for the bucket.
getResourceProfileResponse_statistics :: Lens.Lens' GetResourceProfileResponse (Prelude.Maybe ResourceStatistics)
getResourceProfileResponse_statistics = Lens.lens (\GetResourceProfileResponse' {statistics} -> statistics) (\s@GetResourceProfileResponse' {} a -> s {statistics = a} :: GetResourceProfileResponse)

-- | The response's http status code.
getResourceProfileResponse_httpStatus :: Lens.Lens' GetResourceProfileResponse Prelude.Int
getResourceProfileResponse_httpStatus = Lens.lens (\GetResourceProfileResponse' {httpStatus} -> httpStatus) (\s@GetResourceProfileResponse' {} a -> s {httpStatus = a} :: GetResourceProfileResponse)

instance Prelude.NFData GetResourceProfileResponse where
  rnf GetResourceProfileResponse' {..} =
    Prelude.rnf profileUpdatedAt
      `Prelude.seq` Prelude.rnf sensitivityScore
      `Prelude.seq` Prelude.rnf sensitivityScoreOverridden
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf httpStatus
