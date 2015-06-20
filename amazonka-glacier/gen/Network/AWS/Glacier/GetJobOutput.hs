{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Glacier.GetJobOutput
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

-- | This operation downloads the output of the job you initiated using
-- InitiateJob. Depending on the job type you specified when you initiated
-- the job, the output will be either the content of an archive or a vault
-- inventory.
--
-- A job ID will not expire for at least 24 hours after Amazon Glacier
-- completes the job. That is, you can download the job output within the
-- 24 hours period after Amazon Glacier completes the job.
--
-- If the job output is large, then you can use the @Range@ request header
-- to retrieve a portion of the output. This allows you to download the
-- entire output in smaller chunks of bytes. For example, suppose you have
-- 1 GB of job output you want to download and you decide to download 128
-- MB chunks of data at a time, which is a total of eight Get Job Output
-- requests. You use the following process to download the job output:
--
-- 1.  Download a 128 MB chunk of output by specifying the appropriate byte
--     range using the @Range@ header.
--
-- 2.  Along with the data, the response includes a SHA256 tree hash of the
--     payload. You compute the checksum of the payload on the client and
--     compare it with the checksum you received in the response to ensure
--     you received all the expected data.
--
-- 3.  Repeat steps 1 and 2 for all the eight 128 MB chunks of output data,
--     each time specifying the appropriate byte range.
--
-- 4.  After downloading all the parts of the job output, you have a list
--     of eight checksum values. Compute the tree hash of these values to
--     find the checksum of the entire output. Using the DescribeJob API,
--     obtain job information of the job that provided you the output. The
--     response includes the checksum of the entire archive stored in
--     Amazon Glacier. You compare this value with the checksum you
--     computed to ensure you have downloaded the entire archive content
--     with no errors.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and the underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventory>,
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/downloading-an-archive.html Downloading an Archive>,
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-job-output-get.html Get Job Output>
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-GetJobOutput.html>
module Network.AWS.Glacier.GetJobOutput
    (
    -- * Request
      GetJobOutput
    -- ** Request constructor
    , getJobOutput
    -- ** Request lenses
    , gjoRange
    , gjoAccountId
    , gjoVaultName
    , gjoJobId

    -- * Response
    , GetJobOutputResponse
    -- ** Response constructor
    , getJobOutputResponse
    -- ** Response lenses
    , gjorStatus
    , gjorChecksum
    , gjorAcceptRanges
    , gjorArchiveDescription
    , gjorContentRange
    , gjorContentType
    , gjorBody
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJobOutput' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gjoRange'
--
-- * 'gjoAccountId'
--
-- * 'gjoVaultName'
--
-- * 'gjoJobId'
data GetJobOutput = GetJobOutput'{_gjoRange :: Maybe Text, _gjoAccountId :: Text, _gjoVaultName :: Text, _gjoJobId :: Text} deriving (Eq, Read, Show)

-- | 'GetJobOutput' smart constructor.
getJobOutput :: Text -> Text -> Text -> GetJobOutput
getJobOutput pAccountId pVaultName pJobId = GetJobOutput'{_gjoRange = Nothing, _gjoAccountId = pAccountId, _gjoVaultName = pVaultName, _gjoJobId = pJobId};

-- | The range of bytes to retrieve from the output. For example, if you want
-- to download the first 1,048,576 bytes, specify \"Range:
-- bytes=0-1048575\". By default, this operation downloads the entire
-- output.
gjoRange :: Lens' GetJobOutput (Maybe Text)
gjoRange = lens _gjoRange (\ s a -> s{_gjoRange = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
gjoAccountId :: Lens' GetJobOutput Text
gjoAccountId = lens _gjoAccountId (\ s a -> s{_gjoAccountId = a});

-- | The name of the vault.
gjoVaultName :: Lens' GetJobOutput Text
gjoVaultName = lens _gjoVaultName (\ s a -> s{_gjoVaultName = a});

-- | The job ID whose data is downloaded.
gjoJobId :: Lens' GetJobOutput Text
gjoJobId = lens _gjoJobId (\ s a -> s{_gjoJobId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest GetJobOutput where
        type Sv GetJobOutput = Glacier
        type Rs GetJobOutput = GetJobOutputResponse
        request = get
        response
          = receiveBody
              (\ s h x ->
                 GetJobOutputResponse' <$>
                   (pure (Just s)) <*> (h .#? "x-amz-sha256-tree-hash")
                     <*> (h .#? "Accept-Ranges")
                     <*> (h .#? "x-amz-archive-description")
                     <*> (h .#? "Content-Range")
                     <*> (h .#? "Content-Type")
                     <*> (pure x))

instance ToHeaders GetJobOutput where
        toHeaders GetJobOutput'{..}
          = mconcat ["Range" =# _gjoRange]

instance ToPath GetJobOutput where
        toPath GetJobOutput'{..}
          = mconcat
              ["/", toText _gjoAccountId, "/vaults/",
               toText _gjoVaultName, "/jobs/", toText _gjoJobId,
               "/output"]

instance ToQuery GetJobOutput where
        toQuery = const mempty

-- | /See:/ 'getJobOutputResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gjorStatus'
--
-- * 'gjorChecksum'
--
-- * 'gjorAcceptRanges'
--
-- * 'gjorArchiveDescription'
--
-- * 'gjorContentRange'
--
-- * 'gjorContentType'
--
-- * 'gjorBody'
data GetJobOutputResponse = GetJobOutputResponse'{_gjorStatus :: Maybe Int, _gjorChecksum :: Maybe Text, _gjorAcceptRanges :: Maybe Text, _gjorArchiveDescription :: Maybe Text, _gjorContentRange :: Maybe Text, _gjorContentType :: Maybe Text, _gjorBody :: RsBody} deriving Show

-- | 'GetJobOutputResponse' smart constructor.
getJobOutputResponse :: RsBody -> GetJobOutputResponse
getJobOutputResponse pBody = GetJobOutputResponse'{_gjorStatus = Nothing, _gjorChecksum = Nothing, _gjorAcceptRanges = Nothing, _gjorArchiveDescription = Nothing, _gjorContentRange = Nothing, _gjorContentType = Nothing, _gjorBody = pBody};

-- | The HTTP response code for a job output request. The value depends on
-- whether a range was specified in the request.
gjorStatus :: Lens' GetJobOutputResponse (Maybe Int)
gjorStatus = lens _gjorStatus (\ s a -> s{_gjorStatus = a});

-- | The checksum of the data in the response. This header is returned only
-- when retrieving the output for an archive retrieval job. Furthermore,
-- this header appears only under the following conditions:
--
-- -   You get the entire range of the archive.
-- -   You request a range to return of the archive that starts and ends on
--     a multiple of 1 MB. For example, if you have an 3.1 MB archive and
--     you specify a range to return that starts at 1 MB and ends at 2 MB,
--     then the x-amz-sha256-tree-hash is returned as a response header.
-- -   You request a range of the archive to return that starts on a
--     multiple of 1 MB and goes to the end of the archive. For example, if
--     you have a 3.1 MB archive and you specify a range that starts at 2
--     MB and ends at 3.1 MB (the end of the archive), then the
--     x-amz-sha256-tree-hash is returned as a response header.
gjorChecksum :: Lens' GetJobOutputResponse (Maybe Text)
gjorChecksum = lens _gjorChecksum (\ s a -> s{_gjorChecksum = a});

-- | Indicates the range units accepted. For more information, go to
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC2616>.
gjorAcceptRanges :: Lens' GetJobOutputResponse (Maybe Text)
gjorAcceptRanges = lens _gjorAcceptRanges (\ s a -> s{_gjorAcceptRanges = a});

-- | The description of an archive.
gjorArchiveDescription :: Lens' GetJobOutputResponse (Maybe Text)
gjorArchiveDescription = lens _gjorArchiveDescription (\ s a -> s{_gjorArchiveDescription = a});

-- | The range of bytes returned by Amazon Glacier. If only partial output is
-- downloaded, the response provides the range of bytes Amazon Glacier
-- returned. For example, bytes 0-1048575\/8388608 returns the first 1 MB
-- from 8 MB.
gjorContentRange :: Lens' GetJobOutputResponse (Maybe Text)
gjorContentRange = lens _gjorContentRange (\ s a -> s{_gjorContentRange = a});

-- | The Content-Type depends on whether the job output is an archive or a
-- vault inventory. For archive data, the Content-Type is
-- application\/octet-stream. For vault inventory, if you requested CSV
-- format when you initiated the job, the Content-Type is text\/csv.
-- Otherwise, by default, vault inventory is returned as JSON, and the
-- Content-Type is application\/json.
gjorContentType :: Lens' GetJobOutputResponse (Maybe Text)
gjorContentType = lens _gjorContentType (\ s a -> s{_gjorContentType = a});

-- | The job data, either archive data or inventory data.
gjorBody :: Lens' GetJobOutputResponse RsBody
gjorBody = lens _gjorBody (\ s a -> s{_gjorBody = a});
