{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetJobOutput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation downloads the output of the job you initiated using 'InitiateJob' . Depending on the job type you specified when you initiated the job, the output will be either the content of an archive or a vault inventory.
--
--
-- You can download all the job output or download a portion of the output by specifying a byte range. In the case of an archive retrieval job, depending on the byte range you specify, Amazon Glacier returns the checksum for the portion of the data. You can compute the checksum on the client and verify that the values match to ensure the portion you downloaded is the correct data.
--
-- A job ID will not expire for at least 24 hours after Amazon Glacier completes the job. That a byte range. For both archive and inventory retrieval jobs, you should verify the downloaded size against the size returned in the headers from the __Get Job Output__ response.
--
-- For archive retrieval jobs, you should also verify that the size is what you expected. If you download a portion of the output, the expected size is based on the range of bytes you specified. For example, if you specify a range of @bytes=0-1048575@ , you should verify your download size is 1,048,576 bytes. If you download an entire archive, the expected size is the size of the archive when you uploaded it to Amazon Glacier The expected size is also returned in the headers from the __Get Job Output__ response.
--
-- In the case of an archive retrieval job, depending on the byte range you specify, Amazon Glacier returns the checksum for the portion of the data. To ensure the portion you downloaded is the correct data, compute the checksum on the client, verify that the values match, and verify that the size is what you expected.
--
-- A job ID does not expire for at least 24 hours after Amazon Glacier completes the job. That is, you can download the job output within the 24 hours period after Amazon Glacier completes the job.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and the underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventory> , <http://docs.aws.amazon.com/amazonglacier/latest/dev/downloading-an-archive.html Downloading an Archive> , and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-job-output-get.html Get Job Output >
--
module Network.AWS.Glacier.GetJobOutput
    (
    -- * Creating a Request
      getJobOutput
    , GetJobOutput
    -- * Request Lenses
    , gjoRange
    , gjoAccountId
    , gjoVaultName
    , gjoJobId

    -- * Destructuring the Response
    , getJobOutputResponse
    , GetJobOutputResponse
    -- * Response Lenses
    , gjorsChecksum
    , gjorsAcceptRanges
    , gjorsArchiveDescription
    , gjorsContentRange
    , gjorsContentType
    , gjorsStatus
    , gjorsBody
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for downloading output of an Amazon Glacier job.
--
--
--
-- /See:/ 'getJobOutput' smart constructor.
data GetJobOutput = GetJobOutput'
  { _gjoRange     :: !(Maybe Text)
  , _gjoAccountId :: !Text
  , _gjoVaultName :: !Text
  , _gjoJobId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjoRange' - The range of bytes to retrieve from the output. For example, if you want to download the first 1,048,576 bytes, specify the range as @bytes=0-1048575@ . By default, this operation downloads the entire output. If the job output is large, then you can use a range to retrieve a portion of the output. This allows you to download the entire output in smaller chunks of bytes. For example, suppose you have 1 GB of job output you want to download and you decide to download 128 MB chunks of data at a time, which is a total of eight Get Job Output requests. You use the following process to download the job output:     * Download a 128 MB chunk of output by specifying the appropriate byte range. Verify that all 128 MB of data was received.     * Along with the data, the response includes a SHA256 tree hash of the payload. You compute the checksum of the payload on the client and compare it with the checksum you received in the response to ensure you received all the expected data.     * Repeat steps 1 and 2 for all the eight 128 MB chunks of output data, each time specifying the appropriate byte range.     * After downloading all the parts of the job output, you have a list of eight checksum values. Compute the tree hash of these values to find the checksum of the entire output. Using the 'DescribeJob' API, obtain job information of the job that provided you the output. The response includes the checksum of the entire archive stored in Amazon Glacier. You compare this value with the checksum you computed to ensure you have downloaded the entire archive content with no errors.
--
-- * 'gjoAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'gjoVaultName' - The name of the vault.
--
-- * 'gjoJobId' - The job ID whose data is downloaded.
getJobOutput
    :: Text -- ^ 'gjoAccountId'
    -> Text -- ^ 'gjoVaultName'
    -> Text -- ^ 'gjoJobId'
    -> GetJobOutput
getJobOutput pAccountId_ pVaultName_ pJobId_ =
  GetJobOutput'
    { _gjoRange = Nothing
    , _gjoAccountId = pAccountId_
    , _gjoVaultName = pVaultName_
    , _gjoJobId = pJobId_
    }


-- | The range of bytes to retrieve from the output. For example, if you want to download the first 1,048,576 bytes, specify the range as @bytes=0-1048575@ . By default, this operation downloads the entire output. If the job output is large, then you can use a range to retrieve a portion of the output. This allows you to download the entire output in smaller chunks of bytes. For example, suppose you have 1 GB of job output you want to download and you decide to download 128 MB chunks of data at a time, which is a total of eight Get Job Output requests. You use the following process to download the job output:     * Download a 128 MB chunk of output by specifying the appropriate byte range. Verify that all 128 MB of data was received.     * Along with the data, the response includes a SHA256 tree hash of the payload. You compute the checksum of the payload on the client and compare it with the checksum you received in the response to ensure you received all the expected data.     * Repeat steps 1 and 2 for all the eight 128 MB chunks of output data, each time specifying the appropriate byte range.     * After downloading all the parts of the job output, you have a list of eight checksum values. Compute the tree hash of these values to find the checksum of the entire output. Using the 'DescribeJob' API, obtain job information of the job that provided you the output. The response includes the checksum of the entire archive stored in Amazon Glacier. You compare this value with the checksum you computed to ensure you have downloaded the entire archive content with no errors.
gjoRange :: Lens' GetJobOutput (Maybe Text)
gjoRange = lens _gjoRange (\ s a -> s{_gjoRange = a})

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
gjoAccountId :: Lens' GetJobOutput Text
gjoAccountId = lens _gjoAccountId (\ s a -> s{_gjoAccountId = a})

-- | The name of the vault.
gjoVaultName :: Lens' GetJobOutput Text
gjoVaultName = lens _gjoVaultName (\ s a -> s{_gjoVaultName = a})

-- | The job ID whose data is downloaded.
gjoJobId :: Lens' GetJobOutput Text
gjoJobId = lens _gjoJobId (\ s a -> s{_gjoJobId = a})

instance AWSRequest GetJobOutput where
        type Rs GetJobOutput = GetJobOutputResponse
        request = get glacier
        response
          = receiveBody
              (\ s h x ->
                 GetJobOutputResponse' <$>
                   (h .#? "x-amz-sha256-tree-hash") <*>
                     (h .#? "Accept-Ranges")
                     <*> (h .#? "x-amz-archive-description")
                     <*> (h .#? "Content-Range")
                     <*> (h .#? "Content-Type")
                     <*> (pure (fromEnum s))
                     <*> (pure x))

instance Hashable GetJobOutput where

instance NFData GetJobOutput where

instance ToHeaders GetJobOutput where
        toHeaders GetJobOutput'{..}
          = mconcat ["Range" =# _gjoRange]

instance ToPath GetJobOutput where
        toPath GetJobOutput'{..}
          = mconcat
              ["/", toBS _gjoAccountId, "/vaults/",
               toBS _gjoVaultName, "/jobs/", toBS _gjoJobId,
               "/output"]

instance ToQuery GetJobOutput where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
--
--
-- /See:/ 'getJobOutputResponse' smart constructor.
data GetJobOutputResponse = GetJobOutputResponse'
  { _gjorsChecksum           :: !(Maybe Text)
  , _gjorsAcceptRanges       :: !(Maybe Text)
  , _gjorsArchiveDescription :: !(Maybe Text)
  , _gjorsContentRange       :: !(Maybe Text)
  , _gjorsContentType        :: !(Maybe Text)
  , _gjorsStatus             :: !Int
  , _gjorsBody               :: !RsBody
  } deriving (Show, Generic)


-- | Creates a value of 'GetJobOutputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjorsChecksum' - The checksum of the data in the response. This header is returned only when retrieving the output for an archive retrieval job. Furthermore, this header appears only under the following conditions:     * You get the entire range of the archive.     * You request a range to return of the archive that starts and ends on a multiple of 1 MB. For example, if you have an 3.1 MB archive and you specify a range to return that starts at 1 MB and ends at 2 MB, then the x-amz-sha256-tree-hash is returned as a response header.     * You request a range of the archive to return that starts on a multiple of 1 MB and goes to the end of the archive. For example, if you have a 3.1 MB archive and you specify a range that starts at 2 MB and ends at 3.1 MB (the end of the archive), then the x-amz-sha256-tree-hash is returned as a response header.
--
-- * 'gjorsAcceptRanges' - Indicates the range units accepted. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC2616> .
--
-- * 'gjorsArchiveDescription' - The description of an archive.
--
-- * 'gjorsContentRange' - The range of bytes returned by Amazon Glacier. If only partial output is downloaded, the response provides the range of bytes Amazon Glacier returned. For example, bytes 0-1048575/8388608 returns the first 1 MB from 8 MB.
--
-- * 'gjorsContentType' - The Content-Type depends on whether the job output is an archive or a vault inventory. For archive data, the Content-Type is application/octet-stream. For vault inventory, if you requested CSV format when you initiated the job, the Content-Type is text/csv. Otherwise, by default, vault inventory is returned as JSON, and the Content-Type is application/json.
--
-- * 'gjorsStatus' - The HTTP response code for a job output request. The value depends on whether a range was specified in the request.
--
-- * 'gjorsBody' - The job data, either archive data or inventory data.
getJobOutputResponse
    :: Int -- ^ 'gjorsStatus'
    -> RsBody -- ^ 'gjorsBody'
    -> GetJobOutputResponse
getJobOutputResponse pStatus_ pBody_ =
  GetJobOutputResponse'
    { _gjorsChecksum = Nothing
    , _gjorsAcceptRanges = Nothing
    , _gjorsArchiveDescription = Nothing
    , _gjorsContentRange = Nothing
    , _gjorsContentType = Nothing
    , _gjorsStatus = pStatus_
    , _gjorsBody = pBody_
    }


-- | The checksum of the data in the response. This header is returned only when retrieving the output for an archive retrieval job. Furthermore, this header appears only under the following conditions:     * You get the entire range of the archive.     * You request a range to return of the archive that starts and ends on a multiple of 1 MB. For example, if you have an 3.1 MB archive and you specify a range to return that starts at 1 MB and ends at 2 MB, then the x-amz-sha256-tree-hash is returned as a response header.     * You request a range of the archive to return that starts on a multiple of 1 MB and goes to the end of the archive. For example, if you have a 3.1 MB archive and you specify a range that starts at 2 MB and ends at 3.1 MB (the end of the archive), then the x-amz-sha256-tree-hash is returned as a response header.
gjorsChecksum :: Lens' GetJobOutputResponse (Maybe Text)
gjorsChecksum = lens _gjorsChecksum (\ s a -> s{_gjorsChecksum = a})

-- | Indicates the range units accepted. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC2616> .
gjorsAcceptRanges :: Lens' GetJobOutputResponse (Maybe Text)
gjorsAcceptRanges = lens _gjorsAcceptRanges (\ s a -> s{_gjorsAcceptRanges = a})

-- | The description of an archive.
gjorsArchiveDescription :: Lens' GetJobOutputResponse (Maybe Text)
gjorsArchiveDescription = lens _gjorsArchiveDescription (\ s a -> s{_gjorsArchiveDescription = a})

-- | The range of bytes returned by Amazon Glacier. If only partial output is downloaded, the response provides the range of bytes Amazon Glacier returned. For example, bytes 0-1048575/8388608 returns the first 1 MB from 8 MB.
gjorsContentRange :: Lens' GetJobOutputResponse (Maybe Text)
gjorsContentRange = lens _gjorsContentRange (\ s a -> s{_gjorsContentRange = a})

-- | The Content-Type depends on whether the job output is an archive or a vault inventory. For archive data, the Content-Type is application/octet-stream. For vault inventory, if you requested CSV format when you initiated the job, the Content-Type is text/csv. Otherwise, by default, vault inventory is returned as JSON, and the Content-Type is application/json.
gjorsContentType :: Lens' GetJobOutputResponse (Maybe Text)
gjorsContentType = lens _gjorsContentType (\ s a -> s{_gjorsContentType = a})

-- | The HTTP response code for a job output request. The value depends on whether a range was specified in the request.
gjorsStatus :: Lens' GetJobOutputResponse Int
gjorsStatus = lens _gjorsStatus (\ s a -> s{_gjorsStatus = a})

-- | The job data, either archive data or inventory data.
gjorsBody :: Lens' GetJobOutputResponse RsBody
gjorsBody = lens _gjorsBody (\ s a -> s{_gjorsBody = a})
