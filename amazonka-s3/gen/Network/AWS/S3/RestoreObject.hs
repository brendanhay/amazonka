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
-- Module      : Network.AWS.S3.RestoreObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an archived copy of an object back into Amazon S3
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- This action performs the following types of requests:
--
-- -   @select@ - Perform a select query on an archived object
--
-- -   @restore an archive@ - Restore an archived object
--
-- To use this operation, you must have permissions to perform the
-- @s3:RestoreObject@ action. The bucket owner has this permission by
-- default and can grant this permission to others. For more information
-- about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- __Querying Archives with Select Requests__
--
-- You use a select type of request to perform SQL queries on archived
-- objects. The archived objects that are being queried by the select
-- request must be formatted as uncompressed comma-separated values (CSV)
-- files. You can run queries and custom analytics on your archived data
-- without having to restore your data to a hotter Amazon S3 tier. For an
-- overview about select requests, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/querying-glacier-archives.html Querying Archived Objects>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When making a select request, do the following:
--
-- -   Define an output location for the select query\'s output. This must
--     be an Amazon S3 bucket in the same AWS Region as the bucket that
--     contains the archive object that is being queried. The AWS account
--     that initiates the job must have permissions to write to the S3
--     bucket. You can specify the storage class and encryption for the
--     output objects stored in the bucket. For more information about
--     output, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/querying-glacier-archives.html Querying Archived Objects>
--     in the /Amazon Simple Storage Service Developer Guide/.
--
--     For more information about the @S3@ structure in the request body,
--     see the following:
--
--     -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--     -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Managing Access with ACLs>
--         in the /Amazon Simple Storage Service Developer Guide/
--
--     -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption>
--         in the /Amazon Simple Storage Service Developer Guide/
--
-- -   Define the SQL expression for the @SELECT@ type of restoration for
--     your query in the request body\'s @SelectParameters@ structure. You
--     can use expressions like the following examples.
--
--     -   The following expression returns all records from the specified
--         object.
--
--         @SELECT * FROM Object@
--
--     -   Assuming that you are not using any headers for data stored in
--         the object, you can specify columns with positional headers.
--
--         @SELECT s._1, s._2 FROM Object s WHERE s._3 > 100@
--
--     -   If you have headers and you set the @fileHeaderInfo@ in the
--         @CSV@ structure in the request body to @USE@, you can specify
--         headers in the query. (If you set the @fileHeaderInfo@ field to
--         @IGNORE@, the first row is skipped for the query.) You cannot
--         mix ordinal positions with header column names.
--
--         @SELECT s.Id, s.FirstName, s.SSN FROM S3Object s@
--
-- For more information about using SQL with S3 Glacier Select restore, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-glacier-select-sql-reference.html SQL Reference for Amazon S3 Select and S3 Glacier Select>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When making a select request, you can also do the following:
--
-- -   To expedite your queries, specify the @Expedited@ tier. For more
--     information about tiers, see \"Restoring Archives,\" later in this
--     topic.
--
-- -   Specify details about the data serialization format of both the
--     input object that is being queried and the serialization of the
--     CSV-encoded query results.
--
-- The following are additional important facts about the select feature:
--
-- -   The output results are new Amazon S3 objects. Unlike archive
--     retrievals, they are stored until explicitly deleted-manually or
--     through a lifecycle policy.
--
-- -   You can issue more than one select request on the same Amazon S3
--     object. Amazon S3 doesn\'t deduplicate requests, so avoid issuing
--     duplicate requests.
--
-- -   Amazon S3 accepts a select request even if the object has already
--     been restored. A select request doesn’t return error response @409@.
--
-- __Restoring objects__
--
-- Objects that you archive to the S3 Glacier or S3 Glacier Deep Archive
-- storage class, and S3 Intelligent-Tiering Archive or S3
-- Intelligent-Tiering Deep Archive tiers are not accessible in real time.
-- For objects in Archive Access or Deep Archive Access tiers you must
-- first initiate a restore request, and then wait until the object is
-- moved into the Frequent Access tier. For objects in S3 Glacier or S3
-- Glacier Deep Archive storage classes you must first initiate a restore
-- request, and then wait until a temporary copy of the object is
-- available. To access an archived object, you must restore the object for
-- the duration (number of days) that you specify.
--
-- To restore a specific object version, you can provide a version ID. If
-- you don\'t provide a version ID, Amazon S3 restores the current version.
--
-- When restoring an archived object (or using a select request), you can
-- specify one of the following data access tier options in the @Tier@
-- element of the request body:
--
-- -   __@Expedited@__ - Expedited retrievals allow you to quickly access
--     your data stored in the S3 Glacier storage class or S3
--     Intelligent-Tiering Archive tier when occasional urgent requests for
--     a subset of archives are required. For all but the largest archived
--     objects (250 MB+), data accessed using Expedited retrievals is
--     typically made available within 1–5 minutes. Provisioned capacity
--     ensures that retrieval capacity for Expedited retrievals is
--     available when you need it. Expedited retrievals and provisioned
--     capacity are not available for objects stored in the S3 Glacier Deep
--     Archive storage class or S3 Intelligent-Tiering Deep Archive tier.
--
-- -   __@Standard@__ - Standard retrievals allow you to access any of your
--     archived objects within several hours. This is the default option
--     for retrieval requests that do not specify the retrieval option.
--     Standard retrievals typically finish within 3–5 hours for objects
--     stored in the S3 Glacier storage class or S3 Intelligent-Tiering
--     Archive tier. They typically finish within 12 hours for objects
--     stored in the S3 Glacier Deep Archive storage class or S3
--     Intelligent-Tiering Deep Archive tier. Standard retrievals are free
--     for objects stored in S3 Intelligent-Tiering.
--
-- -   __@Bulk@__ - Bulk retrievals are the lowest-cost retrieval option in
--     S3 Glacier, enabling you to retrieve large amounts, even petabytes,
--     of data inexpensively. Bulk retrievals typically finish within 5–12
--     hours for objects stored in the S3 Glacier storage class or S3
--     Intelligent-Tiering Archive tier. They typically finish within 48
--     hours for objects stored in the S3 Glacier Deep Archive storage
--     class or S3 Intelligent-Tiering Deep Archive tier. Bulk retrievals
--     are free for objects stored in S3 Intelligent-Tiering.
--
-- For more information about archive retrieval options and provisioned
-- capacity for @Expedited@ data access, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/restoring-objects.html Restoring Archived Objects>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- You can use Amazon S3 restore speed upgrade to change the restore speed
-- to a faster speed while it is in progress. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/restoring-objects.html#restoring-objects-upgrade-tier.title.html Upgrading the speed of an in-progress restore>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- To get the status of object restoration, you can send a @HEAD@ request.
-- Operations return the @x-amz-restore@ header, which provides information
-- about the restoration status, in the response. You can use Amazon S3
-- event notifications to notify you when a restore is initiated or
-- completed. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Amazon S3 Event Notifications>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- After restoring an archived object, you can update the restoration
-- period by reissuing the request with a new period. Amazon S3 updates the
-- restoration period relative to the current time and charges only for the
-- request-there are no data transfer charges. You cannot update the
-- restoration period when Amazon S3 is actively processing your current
-- restore request for the object.
--
-- If your bucket has a lifecycle configuration with a rule that includes
-- an expiration action, the object expiration overrides the life span that
-- you specify in a restore request. For example, if you restore an object
-- copy for 10 days, but the object is scheduled to expire in 3 days,
-- Amazon S3 deletes the object in 3 days. For more information about
-- lifecycle configuration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management>
-- in /Amazon Simple Storage Service Developer Guide/.
--
-- __Responses__
--
-- A successful operation returns either the @200 OK@ or @202 Accepted@
-- status code.
--
-- -   If the object is not previously restored, then Amazon S3 returns
--     @202 Accepted@ in the response.
--
-- -   If the object is previously restored, Amazon S3 returns @200 OK@ in
--     the response.
--
-- __Special Errors__
--
-- -   -   /Code: RestoreAlreadyInProgress/
--
--     -   /Cause: Object restore is already in progress. (This error does
--         not apply to SELECT type requests.)/
--
--     -   /HTTP Status Code: 409 Conflict/
--
--     -   /SOAP Fault Code Prefix: Client/
--
-- -   -   /Code: GlacierExpeditedRetrievalNotAvailable/
--
--     -   /Cause: expedited retrievals are currently not available. Try
--         again later. (Returned if there is insufficient capacity to
--         process the Expedited request. This error applies only to
--         Expedited retrievals and not to S3 Standard or Bulk
--         retrievals.)/
--
--     -   /HTTP Status Code: 503/
--
--     -   /SOAP Fault Code Prefix: N\/A/
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketNotificationConfiguration.html GetBucketNotificationConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-glacier-select-sql-reference.html SQL Reference for Amazon S3 Select and S3 Glacier Select>
--     in the /Amazon Simple Storage Service Developer Guide/
module Network.AWS.S3.RestoreObject
  ( -- * Creating a Request
    RestoreObject (..),
    newRestoreObject,

    -- * Request Lenses
    restoreObject_expectedBucketOwner,
    restoreObject_versionId,
    restoreObject_restoreRequest,
    restoreObject_requestPayer,
    restoreObject_bucket,
    restoreObject_key,

    -- * Destructuring the Response
    RestoreObjectResponse (..),
    newRestoreObjectResponse,

    -- * Response Lenses
    restoreObjectResponse_requestCharged,
    restoreObjectResponse_restoreOutputPath,
    restoreObjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newRestoreObject' smart constructor.
data RestoreObject = RestoreObject'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    restoreRequest :: Prelude.Maybe RestoreRequest,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The bucket name containing the object to restore.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- When using this API with Amazon S3 on Outposts, you must direct requests
    -- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
    -- form
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this operation using S3 on Outposts through the AWS SDKs, you
    -- provide the Outposts bucket ARN in place of the bucket name. For more
    -- information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | Object key for which the operation was initiated.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'restoreObject_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'versionId', 'restoreObject_versionId' - VersionId used to reference a specific version of the object.
--
-- 'restoreRequest', 'restoreObject_restoreRequest' - Undocumented member.
--
-- 'requestPayer', 'restoreObject_requestPayer' - Undocumented member.
--
-- 'bucket', 'restoreObject_bucket' - The bucket name containing the object to restore.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'key', 'restoreObject_key' - Object key for which the operation was initiated.
newRestoreObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  RestoreObject
newRestoreObject pBucket_ pKey_ =
  RestoreObject'
    { expectedBucketOwner =
        Prelude.Nothing,
      versionId = Prelude.Nothing,
      restoreRequest = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
restoreObject_expectedBucketOwner :: Lens.Lens' RestoreObject (Prelude.Maybe Prelude.Text)
restoreObject_expectedBucketOwner = Lens.lens (\RestoreObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@RestoreObject' {} a -> s {expectedBucketOwner = a} :: RestoreObject)

-- | VersionId used to reference a specific version of the object.
restoreObject_versionId :: Lens.Lens' RestoreObject (Prelude.Maybe ObjectVersionId)
restoreObject_versionId = Lens.lens (\RestoreObject' {versionId} -> versionId) (\s@RestoreObject' {} a -> s {versionId = a} :: RestoreObject)

-- | Undocumented member.
restoreObject_restoreRequest :: Lens.Lens' RestoreObject (Prelude.Maybe RestoreRequest)
restoreObject_restoreRequest = Lens.lens (\RestoreObject' {restoreRequest} -> restoreRequest) (\s@RestoreObject' {} a -> s {restoreRequest = a} :: RestoreObject)

-- | Undocumented member.
restoreObject_requestPayer :: Lens.Lens' RestoreObject (Prelude.Maybe RequestPayer)
restoreObject_requestPayer = Lens.lens (\RestoreObject' {requestPayer} -> requestPayer) (\s@RestoreObject' {} a -> s {requestPayer = a} :: RestoreObject)

-- | The bucket name containing the object to restore.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
restoreObject_bucket :: Lens.Lens' RestoreObject BucketName
restoreObject_bucket = Lens.lens (\RestoreObject' {bucket} -> bucket) (\s@RestoreObject' {} a -> s {bucket = a} :: RestoreObject)

-- | Object key for which the operation was initiated.
restoreObject_key :: Lens.Lens' RestoreObject ObjectKey
restoreObject_key = Lens.lens (\RestoreObject' {key} -> key) (\s@RestoreObject' {} a -> s {key = a} :: RestoreObject)

instance Core.AWSRequest RestoreObject where
  type
    AWSResponse RestoreObject =
      RestoreObjectResponse
  request = Request.postXML defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RestoreObjectResponse'
            Prelude.<$> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (h Core..#? "x-amz-restore-output-path")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreObject

instance Prelude.NFData RestoreObject

instance Core.ToElement RestoreObject where
  toElement RestoreObject' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}RestoreRequest"
      restoreRequest

instance Core.ToHeaders RestoreObject where
  toHeaders RestoreObject' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath RestoreObject where
  toPath RestoreObject' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery RestoreObject where
  toQuery RestoreObject' {..} =
    Prelude.mconcat
      ["versionId" Core.=: versionId, "restore"]

-- | /See:/ 'newRestoreObjectResponse' smart constructor.
data RestoreObjectResponse = RestoreObjectResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | Indicates the path in the provided S3 output location where Select
    -- results will be restored to.
    restoreOutputPath :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'restoreObjectResponse_requestCharged' - Undocumented member.
--
-- 'restoreOutputPath', 'restoreObjectResponse_restoreOutputPath' - Indicates the path in the provided S3 output location where Select
-- results will be restored to.
--
-- 'httpStatus', 'restoreObjectResponse_httpStatus' - The response's http status code.
newRestoreObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreObjectResponse
newRestoreObjectResponse pHttpStatus_ =
  RestoreObjectResponse'
    { requestCharged =
        Prelude.Nothing,
      restoreOutputPath = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreObjectResponse_requestCharged :: Lens.Lens' RestoreObjectResponse (Prelude.Maybe RequestCharged)
restoreObjectResponse_requestCharged = Lens.lens (\RestoreObjectResponse' {requestCharged} -> requestCharged) (\s@RestoreObjectResponse' {} a -> s {requestCharged = a} :: RestoreObjectResponse)

-- | Indicates the path in the provided S3 output location where Select
-- results will be restored to.
restoreObjectResponse_restoreOutputPath :: Lens.Lens' RestoreObjectResponse (Prelude.Maybe Prelude.Text)
restoreObjectResponse_restoreOutputPath = Lens.lens (\RestoreObjectResponse' {restoreOutputPath} -> restoreOutputPath) (\s@RestoreObjectResponse' {} a -> s {restoreOutputPath = a} :: RestoreObjectResponse)

-- | The response's http status code.
restoreObjectResponse_httpStatus :: Lens.Lens' RestoreObjectResponse Prelude.Int
restoreObjectResponse_httpStatus = Lens.lens (\RestoreObjectResponse' {httpStatus} -> httpStatus) (\s@RestoreObjectResponse' {} a -> s {httpStatus = a} :: RestoreObjectResponse)

instance Prelude.NFData RestoreObjectResponse
