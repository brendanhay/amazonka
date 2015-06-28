{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Glacier.InitiateJob
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation initiates a job of the specified type. In this release,
-- you can initiate a job to retrieve either an archive or a vault
-- inventory (a list of archives in a vault).
--
-- Retrieving data from Amazon Glacier is a two-step process:
--
-- 1.  Initiate a retrieval job.
--
--     A data retrieval policy can cause your initiate retrieval job
--     request to fail with a PolicyEnforcedException exception. For more
--     information about data retrieval policies, see
--     <http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies>.
--     For more information about the PolicyEnforcedException exception,
--     see
--     <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-error-responses.html Error Responses>.
--
-- 2.  After the job completes, download the bytes.
--
-- The retrieval request is executed asynchronously. When you initiate a
-- retrieval job, Amazon Glacier creates a job and returns a job ID in the
-- response. When Amazon Glacier completes the job, you can get the job
-- output (archive or inventory data). For information about getting job
-- output, see GetJobOutput operation.
--
-- The job must complete before you can get its output. To determine when a
-- job is complete, you have the following options:
--
-- -   __Use Amazon SNS Notification__ You can specify an Amazon Simple
--     Notification Service (Amazon SNS) topic to which Amazon Glacier can
--     post a notification after the job is completed. You can specify an
--     SNS topic per job request. The notification is sent only after
--     Amazon Glacier completes the job. In addition to specifying an SNS
--     topic per job request, you can configure vault notifications for a
--     vault so that job notifications are always sent. For more
--     information, see SetVaultNotifications.
--
-- -   __Get job details__ You can make a DescribeJob request to obtain job
--     status information while a job is in progress. However, it is more
--     efficient to use an Amazon SNS notification to determine when a job
--     is complete.
--
-- The information you get via notification is same that you get by calling
-- DescribeJob.
--
-- If for a specific event, you add both the notification configuration on
-- the vault and also specify an SNS topic in your initiate job request,
-- Amazon Glacier sends both notifications. For more information, see
-- SetVaultNotifications.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- __About the Vault Inventory__
--
-- Amazon Glacier prepares an inventory for each vault periodically, every
-- 24 hours. When you initiate a job for a vault inventory, Amazon Glacier
-- returns the last inventory for the vault. The inventory data you get
-- might be up to a day or two days old. Also, the initiate inventory job
-- might take some time to complete before you can download the vault
-- inventory. So you do not want to retrieve a vault inventory for each
-- vault operation. However, in some scenarios, you might find the vault
-- inventory useful. For example, when you upload an archive, you can
-- provide an archive description but not an archive name. Amazon Glacier
-- provides you a unique archive ID, an opaque string of characters. So,
-- you might maintain your own database that maps archive names to their
-- corresponding Amazon Glacier assigned archive IDs. You might find the
-- vault inventory useful in the event you need to reconcile information in
-- your database with the actual vault inventory.
--
-- __Range Inventory Retrieval__
--
-- You can limit the number of inventory items retrieved by filtering on
-- the archive creation date or by setting a limit.
--
-- /Filtering by Archive Creation Date/
--
-- You can retrieve inventory items for archives created between
-- @StartDate@ and @EndDate@ by specifying values for these parameters in
-- the __InitiateJob__ request. Archives created on or after the
-- @StartDate@ and before the @EndDate@ will be returned. If you only
-- provide the @StartDate@ without the @EndDate@, you will retrieve the
-- inventory for all archives created on or after the @StartDate@. If you
-- only provide the @EndDate@ without the @StartDate@, you will get back
-- the inventory for all archives created before the @EndDate@.
--
-- /Limiting Inventory Items per Retrieval/
--
-- You can limit the number of inventory items returned by setting the
-- @Limit@ parameter in the __InitiateJob__ request. The inventory job
-- output will contain inventory items up to the specified @Limit@. If
-- there are more inventory items available, the result is paginated. After
-- a job is complete you can use the DescribeJob operation to get a marker
-- that you use in a subsequent __InitiateJob__ request. The marker will
-- indicate the starting point to retrieve the next set of inventory items.
-- You can page through your entire inventory by repeatedly making
-- __InitiateJob__ requests with the marker from the previous
-- __DescribeJob__ output, until you get a marker from __DescribeJob__ that
-- returns null, indicating that there are no more inventory items
-- available.
--
-- You can use the @Limit@ parameter together with the date range
-- parameters.
--
-- __About Ranged Archive Retrieval__
--
-- You can initiate an archive retrieval for the whole archive or a range
-- of the archive. In the case of ranged archive retrieval, you specify a
-- byte range to return or the whole archive. The range specified must be
-- megabyte (MB) aligned, that is the range start value must be divisible
-- by 1 MB and range end value plus 1 must be divisible by 1 MB or equal
-- the end of the archive. If the ranged archive retrieval is not megabyte
-- aligned, this operation returns a 400 response. Furthermore, to ensure
-- you get checksum values for data you download using Get Job Output API,
-- the range must be tree hash aligned.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and the underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventory>
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-InitiateJob.html>
module Network.AWS.Glacier.InitiateJob
    (
    -- * Request
      InitiateJob
    -- ** Request constructor
    , initiateJob
    -- ** Request lenses
    , ijJobParameters
    , ijAccountId
    , ijVaultName

    -- * Response
    , InitiateJobResponse
    -- ** Response constructor
    , initiateJobResponse
    -- ** Response lenses
    , ijrJobId
    , ijrLocation
    , ijrStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for initiating an Amazon Glacier job.
--
-- /See:/ 'initiateJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ijJobParameters'
--
-- * 'ijAccountId'
--
-- * 'ijVaultName'
data InitiateJob = InitiateJob'
    { _ijJobParameters :: !(Maybe JobParameters)
    , _ijAccountId     :: !Text
    , _ijVaultName     :: !Text
    } deriving (Eq,Read,Show)

-- | 'InitiateJob' smart constructor.
initiateJob :: Text -> Text -> InitiateJob
initiateJob pAccountId pVaultName =
    InitiateJob'
    { _ijJobParameters = Nothing
    , _ijAccountId = pAccountId
    , _ijVaultName = pVaultName
    }

-- | Provides options for specifying job information.
ijJobParameters :: Lens' InitiateJob (Maybe JobParameters)
ijJobParameters = lens _ijJobParameters (\ s a -> s{_ijJobParameters = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
ijAccountId :: Lens' InitiateJob Text
ijAccountId = lens _ijAccountId (\ s a -> s{_ijAccountId = a});

-- | The name of the vault.
ijVaultName :: Lens' InitiateJob Text
ijVaultName = lens _ijVaultName (\ s a -> s{_ijVaultName = a});

instance AWSRequest InitiateJob where
        type Sv InitiateJob = Glacier
        type Rs InitiateJob = InitiateJobResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 InitiateJobResponse' <$>
                   (h .#? "x-amz-job-id") <*> (h .#? "Location") <*>
                     (pure s))

instance ToHeaders InitiateJob where
        toHeaders = const mempty

instance ToJSON InitiateJob where
        toJSON InitiateJob'{..}
          = object ["jobParameters" .= _ijJobParameters]

instance ToPath InitiateJob where
        toPath InitiateJob'{..}
          = mconcat
              ["/", toText _ijAccountId, "/vaults/",
               toText _ijVaultName, "/jobs"]

instance ToQuery InitiateJob where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'initiateJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ijrJobId'
--
-- * 'ijrLocation'
--
-- * 'ijrStatus'
data InitiateJobResponse = InitiateJobResponse'
    { _ijrJobId    :: !(Maybe Text)
    , _ijrLocation :: !(Maybe Text)
    , _ijrStatus   :: !Status
    } deriving (Eq,Show)

-- | 'InitiateJobResponse' smart constructor.
initiateJobResponse :: Status -> InitiateJobResponse
initiateJobResponse pStatus =
    InitiateJobResponse'
    { _ijrJobId = Nothing
    , _ijrLocation = Nothing
    , _ijrStatus = pStatus
    }

-- | The ID of the job.
ijrJobId :: Lens' InitiateJobResponse (Maybe Text)
ijrJobId = lens _ijrJobId (\ s a -> s{_ijrJobId = a});

-- | The relative URI path of the job.
ijrLocation :: Lens' InitiateJobResponse (Maybe Text)
ijrLocation = lens _ijrLocation (\ s a -> s{_ijrLocation = a});

-- | FIXME: Undocumented member.
ijrStatus :: Lens' InitiateJobResponse Status
ijrStatus = lens _ijrStatus (\ s a -> s{_ijrStatus = a});
