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
-- Module      : Amazonka.CodeGuruProfiler.GetProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the aggregated profile of a profiling group for a specified time
-- range. Amazon CodeGuru Profiler collects posted agent profiles for a
-- profiling group into aggregated profiles.
--
-- >  <note> <p> Because aggregated profiles expire over time <code>GetProfile</code> is not idempotent. </p> </note> <p> Specify the time range for the requested aggregated profile using 1 or 2 of the following parameters: <code>startTime</code>, <code>endTime</code>, <code>period</code>. The maximum time range allowed is 7 days. If you specify all 3 parameters, an exception is thrown. If you specify only <code>period</code>, the latest aggregated profile is returned. </p> <p> Aggregated profiles are available with aggregation periods of 5 minutes, 1 hour, and 1 day, aligned to UTC. The aggregation period of an aggregated profile determines how long it is retained. For more information, see <a href="https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AggregatedProfileTime.html"> <code>AggregatedProfileTime</code> </a>. The aggregated profile's aggregation period determines how long it is retained by CodeGuru Profiler. </p> <ul> <li> <p> If the aggregation period is 5 minutes, the aggregated profile is retained for 15 days. </p> </li> <li> <p> If the aggregation period is 1 hour, the aggregated profile is retained for 60 days. </p> </li> <li> <p> If the aggregation period is 1 day, the aggregated profile is retained for 3 years. </p> </li> </ul> <p>There are two use cases for calling <code>GetProfile</code>.</p> <ol> <li> <p> If you want to return an aggregated profile that already exists, use <a href="https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ListProfileTimes.html"> <code>ListProfileTimes</code> </a> to view the time ranges of existing aggregated profiles. Use them in a <code>GetProfile</code> request to return a specific, existing aggregated profile. </p> </li> <li> <p> If you want to return an aggregated profile for a time range that doesn't align with an existing aggregated profile, then CodeGuru Profiler makes a best effort to combine existing aggregated profiles from the requested time range and return them as one aggregated profile. </p> <p> If aggregated profiles do not exist for the full time range requested, then aggregated profiles for a smaller time range are returned. For example, if the requested time range is from 00:00 to 00:20, and the existing aggregated profiles are from 00:15 and 00:25, then the aggregated profiles from 00:15 to 00:20 are returned. </p> </li> </ol>
module Amazonka.CodeGuruProfiler.GetProfile
  ( -- * Creating a Request
    GetProfile (..),
    newGetProfile,

    -- * Request Lenses
    getProfile_accept,
    getProfile_endTime,
    getProfile_maxDepth,
    getProfile_period,
    getProfile_startTime,
    getProfile_profilingGroupName,

    -- * Destructuring the Response
    GetProfileResponse (..),
    newGetProfileResponse,

    -- * Response Lenses
    getProfileResponse_contentEncoding,
    getProfileResponse_httpStatus,
    getProfileResponse_contentType,
    getProfileResponse_profile,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the getProfileRequest.
--
-- /See:/ 'newGetProfile' smart constructor.
data GetProfile = GetProfile'
  { -- | The format of the returned profiling data. The format maps to the
    -- @Accept@ and @Content-Type@ headers of the HTTP request. You can specify
    -- one of the following: or the default .
    --
    -- >  <ul> <li> <p> <code>application/json</code> — standard JSON format </p> </li> <li> <p> <code>application/x-amzn-ion</code> — the Amazon Ion data format. For more information, see <a href="http://amzn.github.io/ion-docs/">Amazon Ion</a>. </p> </li> </ul>
    accept :: Prelude.Maybe Prelude.Text,
    -- | The end time of the requested profile. Specify using the ISO 8601
    -- format. For example, 2020-06-01T13:15:02.001Z represents 1 millisecond
    -- past June 1, 2020 1:15:02 PM UTC.
    --
    -- If you specify @endTime@, then you must also specify @period@ or
    -- @startTime@, but not both.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The maximum depth of the stacks in the code that is represented in the
    -- aggregated profile. For example, if CodeGuru Profiler finds a method
    -- @A@, which calls method @B@, which calls method @C@, which calls method
    -- @D@, then the depth is 4. If the @maxDepth@ is set to 2, then the
    -- aggregated profile contains representations of methods @A@ and @B@.
    maxDepth :: Prelude.Maybe Prelude.Natural,
    -- | Used with @startTime@ or @endTime@ to specify the time range for the
    -- returned aggregated profile. Specify using the ISO 8601 format. For
    -- example, @P1DT1H1M1S@.
    --
    -- >  <p> To get the latest aggregated profile, specify only <code>period</code>. </p>
    period :: Prelude.Maybe Prelude.Text,
    -- | The start time of the profile to get. Specify using the ISO 8601 format.
    -- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
    -- 1, 2020 1:15:02 PM UTC.
    --
    -- >  <p> If you specify <code>startTime</code>, then you must also specify <code>period</code> or <code>endTime</code>, but not both. </p>
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the profiling group to get.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accept', 'getProfile_accept' - The format of the returned profiling data. The format maps to the
-- @Accept@ and @Content-Type@ headers of the HTTP request. You can specify
-- one of the following: or the default .
--
-- >  <ul> <li> <p> <code>application/json</code> — standard JSON format </p> </li> <li> <p> <code>application/x-amzn-ion</code> — the Amazon Ion data format. For more information, see <a href="http://amzn.github.io/ion-docs/">Amazon Ion</a>. </p> </li> </ul>
--
-- 'endTime', 'getProfile_endTime' - The end time of the requested profile. Specify using the ISO 8601
-- format. For example, 2020-06-01T13:15:02.001Z represents 1 millisecond
-- past June 1, 2020 1:15:02 PM UTC.
--
-- If you specify @endTime@, then you must also specify @period@ or
-- @startTime@, but not both.
--
-- 'maxDepth', 'getProfile_maxDepth' - The maximum depth of the stacks in the code that is represented in the
-- aggregated profile. For example, if CodeGuru Profiler finds a method
-- @A@, which calls method @B@, which calls method @C@, which calls method
-- @D@, then the depth is 4. If the @maxDepth@ is set to 2, then the
-- aggregated profile contains representations of methods @A@ and @B@.
--
-- 'period', 'getProfile_period' - Used with @startTime@ or @endTime@ to specify the time range for the
-- returned aggregated profile. Specify using the ISO 8601 format. For
-- example, @P1DT1H1M1S@.
--
-- >  <p> To get the latest aggregated profile, specify only <code>period</code>. </p>
--
-- 'startTime', 'getProfile_startTime' - The start time of the profile to get. Specify using the ISO 8601 format.
-- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
-- 1, 2020 1:15:02 PM UTC.
--
-- >  <p> If you specify <code>startTime</code>, then you must also specify <code>period</code> or <code>endTime</code>, but not both. </p>
--
-- 'profilingGroupName', 'getProfile_profilingGroupName' - The name of the profiling group to get.
newGetProfile ::
  -- | 'profilingGroupName'
  Prelude.Text ->
  GetProfile
newGetProfile pProfilingGroupName_ =
  GetProfile'
    { accept = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxDepth = Prelude.Nothing,
      period = Prelude.Nothing,
      startTime = Prelude.Nothing,
      profilingGroupName = pProfilingGroupName_
    }

-- | The format of the returned profiling data. The format maps to the
-- @Accept@ and @Content-Type@ headers of the HTTP request. You can specify
-- one of the following: or the default .
--
-- >  <ul> <li> <p> <code>application/json</code> — standard JSON format </p> </li> <li> <p> <code>application/x-amzn-ion</code> — the Amazon Ion data format. For more information, see <a href="http://amzn.github.io/ion-docs/">Amazon Ion</a>. </p> </li> </ul>
getProfile_accept :: Lens.Lens' GetProfile (Prelude.Maybe Prelude.Text)
getProfile_accept = Lens.lens (\GetProfile' {accept} -> accept) (\s@GetProfile' {} a -> s {accept = a} :: GetProfile)

-- | The end time of the requested profile. Specify using the ISO 8601
-- format. For example, 2020-06-01T13:15:02.001Z represents 1 millisecond
-- past June 1, 2020 1:15:02 PM UTC.
--
-- If you specify @endTime@, then you must also specify @period@ or
-- @startTime@, but not both.
getProfile_endTime :: Lens.Lens' GetProfile (Prelude.Maybe Prelude.UTCTime)
getProfile_endTime = Lens.lens (\GetProfile' {endTime} -> endTime) (\s@GetProfile' {} a -> s {endTime = a} :: GetProfile) Prelude.. Lens.mapping Data._Time

-- | The maximum depth of the stacks in the code that is represented in the
-- aggregated profile. For example, if CodeGuru Profiler finds a method
-- @A@, which calls method @B@, which calls method @C@, which calls method
-- @D@, then the depth is 4. If the @maxDepth@ is set to 2, then the
-- aggregated profile contains representations of methods @A@ and @B@.
getProfile_maxDepth :: Lens.Lens' GetProfile (Prelude.Maybe Prelude.Natural)
getProfile_maxDepth = Lens.lens (\GetProfile' {maxDepth} -> maxDepth) (\s@GetProfile' {} a -> s {maxDepth = a} :: GetProfile)

-- | Used with @startTime@ or @endTime@ to specify the time range for the
-- returned aggregated profile. Specify using the ISO 8601 format. For
-- example, @P1DT1H1M1S@.
--
-- >  <p> To get the latest aggregated profile, specify only <code>period</code>. </p>
getProfile_period :: Lens.Lens' GetProfile (Prelude.Maybe Prelude.Text)
getProfile_period = Lens.lens (\GetProfile' {period} -> period) (\s@GetProfile' {} a -> s {period = a} :: GetProfile)

-- | The start time of the profile to get. Specify using the ISO 8601 format.
-- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
-- 1, 2020 1:15:02 PM UTC.
--
-- >  <p> If you specify <code>startTime</code>, then you must also specify <code>period</code> or <code>endTime</code>, but not both. </p>
getProfile_startTime :: Lens.Lens' GetProfile (Prelude.Maybe Prelude.UTCTime)
getProfile_startTime = Lens.lens (\GetProfile' {startTime} -> startTime) (\s@GetProfile' {} a -> s {startTime = a} :: GetProfile) Prelude.. Lens.mapping Data._Time

-- | The name of the profiling group to get.
getProfile_profilingGroupName :: Lens.Lens' GetProfile Prelude.Text
getProfile_profilingGroupName = Lens.lens (\GetProfile' {profilingGroupName} -> profilingGroupName) (\s@GetProfile' {} a -> s {profilingGroupName = a} :: GetProfile)

instance Core.AWSRequest GetProfile where
  type AWSResponse GetProfile = GetProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetProfileResponse'
            Prelude.<$> (h Data..#? "Content-Encoding")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (h Data..# "Content-Type")
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetProfile where
  hashWithSalt _salt GetProfile' {..} =
    _salt
      `Prelude.hashWithSalt` accept
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxDepth
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData GetProfile where
  rnf GetProfile' {..} =
    Prelude.rnf accept
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxDepth
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf profilingGroupName

instance Data.ToHeaders GetProfile where
  toHeaders GetProfile' {..} =
    Prelude.mconcat
      [ "Accept" Data.=# accept,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetProfile where
  toPath GetProfile' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Data.toBS profilingGroupName,
        "/profile"
      ]

instance Data.ToQuery GetProfile where
  toQuery GetProfile' {..} =
    Prelude.mconcat
      [ "endTime" Data.=: endTime,
        "maxDepth" Data.=: maxDepth,
        "period" Data.=: period,
        "startTime" Data.=: startTime
      ]

-- | The structure representing the getProfileResponse.
--
-- /See:/ 'newGetProfileResponse' smart constructor.
data GetProfileResponse = GetProfileResponse'
  { -- | The content encoding of the profile.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The content type of the profile in the payload. It is either
    -- @application\/json@ or the default @application\/x-amzn-ion@.
    contentType :: Prelude.Text,
    -- | Information about the profile.
    profile :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentEncoding', 'getProfileResponse_contentEncoding' - The content encoding of the profile.
--
-- 'httpStatus', 'getProfileResponse_httpStatus' - The response's http status code.
--
-- 'contentType', 'getProfileResponse_contentType' - The content type of the profile in the payload. It is either
-- @application\/json@ or the default @application\/x-amzn-ion@.
--
-- 'profile', 'getProfileResponse_profile' - Information about the profile.
newGetProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'contentType'
  Prelude.Text ->
  -- | 'profile'
  Prelude.ByteString ->
  GetProfileResponse
newGetProfileResponse
  pHttpStatus_
  pContentType_
  pProfile_ =
    GetProfileResponse'
      { contentEncoding =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        contentType = pContentType_,
        profile = pProfile_
      }

-- | The content encoding of the profile.
getProfileResponse_contentEncoding :: Lens.Lens' GetProfileResponse (Prelude.Maybe Prelude.Text)
getProfileResponse_contentEncoding = Lens.lens (\GetProfileResponse' {contentEncoding} -> contentEncoding) (\s@GetProfileResponse' {} a -> s {contentEncoding = a} :: GetProfileResponse)

-- | The response's http status code.
getProfileResponse_httpStatus :: Lens.Lens' GetProfileResponse Prelude.Int
getProfileResponse_httpStatus = Lens.lens (\GetProfileResponse' {httpStatus} -> httpStatus) (\s@GetProfileResponse' {} a -> s {httpStatus = a} :: GetProfileResponse)

-- | The content type of the profile in the payload. It is either
-- @application\/json@ or the default @application\/x-amzn-ion@.
getProfileResponse_contentType :: Lens.Lens' GetProfileResponse Prelude.Text
getProfileResponse_contentType = Lens.lens (\GetProfileResponse' {contentType} -> contentType) (\s@GetProfileResponse' {} a -> s {contentType = a} :: GetProfileResponse)

-- | Information about the profile.
getProfileResponse_profile :: Lens.Lens' GetProfileResponse Prelude.ByteString
getProfileResponse_profile = Lens.lens (\GetProfileResponse' {profile} -> profile) (\s@GetProfileResponse' {} a -> s {profile = a} :: GetProfileResponse)

instance Prelude.NFData GetProfileResponse where
  rnf GetProfileResponse' {..} =
    Prelude.rnf contentEncoding
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf profile
