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
-- Module      : Amazonka.IAM.GetServiceLastAccessedDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service last accessed report that was created using the
-- @GenerateServiceLastAccessedDetails@ operation. You can use the @JobId@
-- parameter in @GetServiceLastAccessedDetails@ to retrieve the status of
-- your report job. When the report is complete, you can retrieve the
-- generated report. The report includes a list of Amazon Web Services
-- services that the resource (user, group, role, or managed policy) can
-- access.
--
-- Service last accessed data does not use other policy types when
-- determining whether a resource could access a service. These other
-- policy types include resource-based policies, access control lists,
-- Organizations policies, IAM permissions boundaries, and STS assume role
-- policies. It only applies permissions policy logic. For more about the
-- evaluation of policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating policies>
-- in the /IAM User Guide/.
--
-- For each service that the resource could access using permissions
-- policies, the operation returns details about the most recent access
-- attempt. If there was no attempt, the service is listed without details
-- about the most recent attempt to access the service. If the operation
-- fails, the @GetServiceLastAccessedDetails@ operation returns the reason
-- that it failed.
--
-- The @GetServiceLastAccessedDetails@ operation returns a list of
-- services. This list includes the number of entities that have attempted
-- to access the service and the date and time of the last attempt. It also
-- returns the ARN of the following entity, depending on the resource ARN
-- that you used to generate the report:
--
-- -   __User__ – Returns the user ARN that you used to generate the report
--
-- -   __Group__ – Returns the ARN of the group member (user) that last
--     attempted to access the service
--
-- -   __Role__ – Returns the role ARN that you used to generate the report
--
-- -   __Policy__ – Returns the ARN of the user or role that last used the
--     policy to attempt to access the service
--
-- By default, the list is sorted by service namespace.
--
-- If you specified @ACTION_LEVEL@ granularity when you generated the
-- report, this operation returns service and action last accessed data.
-- This includes the most recent access attempt for each tracked action
-- within a service. Otherwise, this operation returns only service data.
--
-- For more information about service and action last accessed data, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html Reducing permissions using service last accessed data>
-- in the /IAM User Guide/.
module Amazonka.IAM.GetServiceLastAccessedDetails
  ( -- * Creating a Request
    GetServiceLastAccessedDetails (..),
    newGetServiceLastAccessedDetails,

    -- * Request Lenses
    getServiceLastAccessedDetails_marker,
    getServiceLastAccessedDetails_maxItems,
    getServiceLastAccessedDetails_jobId,

    -- * Destructuring the Response
    GetServiceLastAccessedDetailsResponse (..),
    newGetServiceLastAccessedDetailsResponse,

    -- * Response Lenses
    getServiceLastAccessedDetailsResponse_error,
    getServiceLastAccessedDetailsResponse_isTruncated,
    getServiceLastAccessedDetailsResponse_jobType,
    getServiceLastAccessedDetailsResponse_marker,
    getServiceLastAccessedDetailsResponse_httpStatus,
    getServiceLastAccessedDetailsResponse_jobStatus,
    getServiceLastAccessedDetailsResponse_jobCreationDate,
    getServiceLastAccessedDetailsResponse_servicesLastAccessed,
    getServiceLastAccessedDetailsResponse_jobCompletionDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceLastAccessedDetails' smart constructor.
data GetServiceLastAccessedDetails = GetServiceLastAccessedDetails'
  { -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the request generated by the
    -- GenerateServiceLastAccessedDetails operation. The @JobId@ returned by
    -- @GenerateServiceLastAccessedDetail@ must be used by the same role within
    -- a session, or by the same user when used to call
    -- @GetServiceLastAccessedDetail@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceLastAccessedDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'getServiceLastAccessedDetails_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'getServiceLastAccessedDetails_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'jobId', 'getServiceLastAccessedDetails_jobId' - The ID of the request generated by the
-- GenerateServiceLastAccessedDetails operation. The @JobId@ returned by
-- @GenerateServiceLastAccessedDetail@ must be used by the same role within
-- a session, or by the same user when used to call
-- @GetServiceLastAccessedDetail@.
newGetServiceLastAccessedDetails ::
  -- | 'jobId'
  Prelude.Text ->
  GetServiceLastAccessedDetails
newGetServiceLastAccessedDetails pJobId_ =
  GetServiceLastAccessedDetails'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      jobId = pJobId_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
getServiceLastAccessedDetails_marker :: Lens.Lens' GetServiceLastAccessedDetails (Prelude.Maybe Prelude.Text)
getServiceLastAccessedDetails_marker = Lens.lens (\GetServiceLastAccessedDetails' {marker} -> marker) (\s@GetServiceLastAccessedDetails' {} a -> s {marker = a} :: GetServiceLastAccessedDetails)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
getServiceLastAccessedDetails_maxItems :: Lens.Lens' GetServiceLastAccessedDetails (Prelude.Maybe Prelude.Natural)
getServiceLastAccessedDetails_maxItems = Lens.lens (\GetServiceLastAccessedDetails' {maxItems} -> maxItems) (\s@GetServiceLastAccessedDetails' {} a -> s {maxItems = a} :: GetServiceLastAccessedDetails)

-- | The ID of the request generated by the
-- GenerateServiceLastAccessedDetails operation. The @JobId@ returned by
-- @GenerateServiceLastAccessedDetail@ must be used by the same role within
-- a session, or by the same user when used to call
-- @GetServiceLastAccessedDetail@.
getServiceLastAccessedDetails_jobId :: Lens.Lens' GetServiceLastAccessedDetails Prelude.Text
getServiceLastAccessedDetails_jobId = Lens.lens (\GetServiceLastAccessedDetails' {jobId} -> jobId) (\s@GetServiceLastAccessedDetails' {} a -> s {jobId = a} :: GetServiceLastAccessedDetails)

instance
  Core.AWSRequest
    GetServiceLastAccessedDetails
  where
  type
    AWSResponse GetServiceLastAccessedDetails =
      GetServiceLastAccessedDetailsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetServiceLastAccessedDetailsResult"
      ( \s h x ->
          GetServiceLastAccessedDetailsResponse'
            Prelude.<$> (x Data..@? "Error")
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "JobType")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "JobStatus")
            Prelude.<*> (x Data..@ "JobCreationDate")
            Prelude.<*> ( x Data..@? "ServicesLastAccessed"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
            Prelude.<*> (x Data..@ "JobCompletionDate")
      )

instance
  Prelude.Hashable
    GetServiceLastAccessedDetails
  where
  hashWithSalt _salt GetServiceLastAccessedDetails' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetServiceLastAccessedDetails where
  rnf GetServiceLastAccessedDetails' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetServiceLastAccessedDetails where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetServiceLastAccessedDetails where
  toPath = Prelude.const "/"

instance Data.ToQuery GetServiceLastAccessedDetails where
  toQuery GetServiceLastAccessedDetails' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetServiceLastAccessedDetails" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "JobId" Data.=: jobId
      ]

-- | /See:/ 'newGetServiceLastAccessedDetailsResponse' smart constructor.
data GetServiceLastAccessedDetailsResponse = GetServiceLastAccessedDetailsResponse'
  { -- | An object that contains details about the reason the operation failed.
    error :: Prelude.Maybe ErrorDetails,
    -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The type of job. Service jobs return information about when each service
    -- was last accessed. Action jobs also include information about when
    -- tracked actions within the service were last accessed.
    jobType :: Prelude.Maybe AccessAdvisorUsageGranularityType,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the job.
    jobStatus :: JobStatusType,
    -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- report job was created.
    jobCreationDate :: Data.ISO8601,
    -- | A @ServiceLastAccessed@ object that contains details about the most
    -- recent attempt to access the service.
    servicesLastAccessed :: [ServiceLastAccessed],
    -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- generated report job was completed or failed.
    --
    -- This field is null if the job is still in progress, as indicated by a
    -- job status value of @IN_PROGRESS@.
    jobCompletionDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceLastAccessedDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'getServiceLastAccessedDetailsResponse_error' - An object that contains details about the reason the operation failed.
--
-- 'isTruncated', 'getServiceLastAccessedDetailsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'jobType', 'getServiceLastAccessedDetailsResponse_jobType' - The type of job. Service jobs return information about when each service
-- was last accessed. Action jobs also include information about when
-- tracked actions within the service were last accessed.
--
-- 'marker', 'getServiceLastAccessedDetailsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'getServiceLastAccessedDetailsResponse_httpStatus' - The response's http status code.
--
-- 'jobStatus', 'getServiceLastAccessedDetailsResponse_jobStatus' - The status of the job.
--
-- 'jobCreationDate', 'getServiceLastAccessedDetailsResponse_jobCreationDate' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- report job was created.
--
-- 'servicesLastAccessed', 'getServiceLastAccessedDetailsResponse_servicesLastAccessed' - A @ServiceLastAccessed@ object that contains details about the most
-- recent attempt to access the service.
--
-- 'jobCompletionDate', 'getServiceLastAccessedDetailsResponse_jobCompletionDate' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- generated report job was completed or failed.
--
-- This field is null if the job is still in progress, as indicated by a
-- job status value of @IN_PROGRESS@.
newGetServiceLastAccessedDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobStatus'
  JobStatusType ->
  -- | 'jobCreationDate'
  Prelude.UTCTime ->
  -- | 'jobCompletionDate'
  Prelude.UTCTime ->
  GetServiceLastAccessedDetailsResponse
newGetServiceLastAccessedDetailsResponse
  pHttpStatus_
  pJobStatus_
  pJobCreationDate_
  pJobCompletionDate_ =
    GetServiceLastAccessedDetailsResponse'
      { error =
          Prelude.Nothing,
        isTruncated = Prelude.Nothing,
        jobType = Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobStatus = pJobStatus_,
        jobCreationDate =
          Data._Time
            Lens.# pJobCreationDate_,
        servicesLastAccessed =
          Prelude.mempty,
        jobCompletionDate =
          Data._Time
            Lens.# pJobCompletionDate_
      }

-- | An object that contains details about the reason the operation failed.
getServiceLastAccessedDetailsResponse_error :: Lens.Lens' GetServiceLastAccessedDetailsResponse (Prelude.Maybe ErrorDetails)
getServiceLastAccessedDetailsResponse_error = Lens.lens (\GetServiceLastAccessedDetailsResponse' {error} -> error) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {error = a} :: GetServiceLastAccessedDetailsResponse)

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
getServiceLastAccessedDetailsResponse_isTruncated :: Lens.Lens' GetServiceLastAccessedDetailsResponse (Prelude.Maybe Prelude.Bool)
getServiceLastAccessedDetailsResponse_isTruncated = Lens.lens (\GetServiceLastAccessedDetailsResponse' {isTruncated} -> isTruncated) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {isTruncated = a} :: GetServiceLastAccessedDetailsResponse)

-- | The type of job. Service jobs return information about when each service
-- was last accessed. Action jobs also include information about when
-- tracked actions within the service were last accessed.
getServiceLastAccessedDetailsResponse_jobType :: Lens.Lens' GetServiceLastAccessedDetailsResponse (Prelude.Maybe AccessAdvisorUsageGranularityType)
getServiceLastAccessedDetailsResponse_jobType = Lens.lens (\GetServiceLastAccessedDetailsResponse' {jobType} -> jobType) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {jobType = a} :: GetServiceLastAccessedDetailsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
getServiceLastAccessedDetailsResponse_marker :: Lens.Lens' GetServiceLastAccessedDetailsResponse (Prelude.Maybe Prelude.Text)
getServiceLastAccessedDetailsResponse_marker = Lens.lens (\GetServiceLastAccessedDetailsResponse' {marker} -> marker) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {marker = a} :: GetServiceLastAccessedDetailsResponse)

-- | The response's http status code.
getServiceLastAccessedDetailsResponse_httpStatus :: Lens.Lens' GetServiceLastAccessedDetailsResponse Prelude.Int
getServiceLastAccessedDetailsResponse_httpStatus = Lens.lens (\GetServiceLastAccessedDetailsResponse' {httpStatus} -> httpStatus) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {httpStatus = a} :: GetServiceLastAccessedDetailsResponse)

-- | The status of the job.
getServiceLastAccessedDetailsResponse_jobStatus :: Lens.Lens' GetServiceLastAccessedDetailsResponse JobStatusType
getServiceLastAccessedDetailsResponse_jobStatus = Lens.lens (\GetServiceLastAccessedDetailsResponse' {jobStatus} -> jobStatus) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {jobStatus = a} :: GetServiceLastAccessedDetailsResponse)

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- report job was created.
getServiceLastAccessedDetailsResponse_jobCreationDate :: Lens.Lens' GetServiceLastAccessedDetailsResponse Prelude.UTCTime
getServiceLastAccessedDetailsResponse_jobCreationDate = Lens.lens (\GetServiceLastAccessedDetailsResponse' {jobCreationDate} -> jobCreationDate) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {jobCreationDate = a} :: GetServiceLastAccessedDetailsResponse) Prelude.. Data._Time

-- | A @ServiceLastAccessed@ object that contains details about the most
-- recent attempt to access the service.
getServiceLastAccessedDetailsResponse_servicesLastAccessed :: Lens.Lens' GetServiceLastAccessedDetailsResponse [ServiceLastAccessed]
getServiceLastAccessedDetailsResponse_servicesLastAccessed = Lens.lens (\GetServiceLastAccessedDetailsResponse' {servicesLastAccessed} -> servicesLastAccessed) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {servicesLastAccessed = a} :: GetServiceLastAccessedDetailsResponse) Prelude.. Lens.coerced

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- generated report job was completed or failed.
--
-- This field is null if the job is still in progress, as indicated by a
-- job status value of @IN_PROGRESS@.
getServiceLastAccessedDetailsResponse_jobCompletionDate :: Lens.Lens' GetServiceLastAccessedDetailsResponse Prelude.UTCTime
getServiceLastAccessedDetailsResponse_jobCompletionDate = Lens.lens (\GetServiceLastAccessedDetailsResponse' {jobCompletionDate} -> jobCompletionDate) (\s@GetServiceLastAccessedDetailsResponse' {} a -> s {jobCompletionDate = a} :: GetServiceLastAccessedDetailsResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    GetServiceLastAccessedDetailsResponse
  where
  rnf GetServiceLastAccessedDetailsResponse' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobCreationDate
      `Prelude.seq` Prelude.rnf servicesLastAccessed
      `Prelude.seq` Prelude.rnf jobCompletionDate
