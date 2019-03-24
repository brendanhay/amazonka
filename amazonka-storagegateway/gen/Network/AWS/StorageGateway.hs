{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Storage Gateway Service__
--
-- AWS Storage Gateway is the service that connects an on-premises software appliance with cloud-based storage to provide seamless and secure integration between an organization's on-premises IT environment and AWS's storage infrastructure. The service enables you to securely upload data to the AWS cloud for cost effective backup and rapid disaster recovery.
--
-- Use the following links to get started using the /AWS Storage Gateway Service API Reference/ :
--
--     * <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewayHTTPRequestsHeaders AWS Storage Gateway Required Request Headers> : Describes the required headers that you must send with every POST request to AWS Storage Gateway.
--
--     * <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewaySigningRequests Signing Requests> : AWS Storage Gateway requires that you authenticate every request you send; this topic describes how sign such a request.
--
--     * <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#APIErrorResponses Error Responses> : Provides reference information about AWS Storage Gateway errors.
--
--     * <https://docs.aws.amazon.com/storagegateway/latest/APIReference/API_Operations.html Operations in AWS Storage Gateway> : Contains detailed descriptions of all AWS Storage Gateway operations, their request parameters, response elements, possible errors, and examples of requests and responses.
--
--     * <http://docs.aws.amazon.com/general/latest/gr/rande.html#sg_region AWS Storage Gateway Regions and Endpoints:> Provides a list of each AWS region and endpoints available for use with AWS Storage Gateway.
--
--
--
-- /Important:/ IDs for Storage Gateway volumes and Amazon EBS snapshots created from gateway volumes are changing to a longer format. Starting in December 2016, all new volumes and snapshots will be created with a 17-character string. Starting in April 2016, you will be able to use these longer IDs so you can test your systems with the new format. For more information, see <https://aws.amazon.com/ec2/faqs/#longer-ids Longer EC2 and EBS Resource IDs> .
--
-- For example, a volume Amazon Resource Name (ARN) with the longer volume ID format looks like the following:
--
-- @arn:aws:storagegateway:us-west-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABBCCDDEEFFG@ .
--
-- A snapshot ID with the longer ID format looks like the following: @snap-78e226633445566ee@ .
--
-- For more information, see <https://forums.aws.amazon.com/ann.jspa?annID=3557 Announcement: Heads-up
