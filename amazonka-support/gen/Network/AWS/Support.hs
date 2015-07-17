{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | AWS Support
--
-- The AWS Support API reference is intended for programmers who need
-- detailed information about the AWS Support operations and data types.
-- This service enables you to manage your AWS Support cases
-- programmatically. It uses HTTP methods that return results in JSON
-- format.
--
-- The AWS Support service also exposes a set of
-- <https://aws.amazon.com/premiumsupport/trustedadvisor/ Trusted Advisor>
-- features. You can retrieve a list of checks and their descriptions, get
-- check results, specify checks to refresh, and get the refresh status of
-- checks.
--
-- The following list describes the AWS Support case management operations:
--
-- -   __Service names, issue categories, and available severity levels.__
--     The DescribeServices and DescribeSeverityLevels operations return
--     AWS service names, service codes, service categories, and problem
--     severity levels. You use these values when you call the CreateCase
--     operation.
-- -   __Case creation, case details, and case resolution.__ The
--     CreateCase, DescribeCases, DescribeAttachment, and ResolveCase
--     operations create AWS Support cases, retrieve information about
--     cases, and resolve cases.
-- -   __Case communication.__ The DescribeCommunications,
--     AddCommunicationToCase, and AddAttachmentsToSet operations retrieve
--     and add communications and attachments to AWS Support cases.
--
-- The following list describes the operations available from the AWS
-- Support service for Trusted Advisor:
--
-- -   DescribeTrustedAdvisorChecks returns the list of checks that run
--     against your AWS resources.
-- -   Using the @CheckId@ for a specific check returned by
--     DescribeTrustedAdvisorChecks, you can call
--     DescribeTrustedAdvisorCheckResult to obtain the results for the
--     check you specified.
-- -   DescribeTrustedAdvisorCheckSummaries returns summarized results for
--     one or more Trusted Advisor checks.
-- -   RefreshTrustedAdvisorCheck requests that Trusted Advisor rerun a
--     specified check.
-- -   DescribeTrustedAdvisorCheckRefreshStatuses reports the refresh
--     status of one or more checks.
--
-- For authentication of requests, AWS Support uses
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- See
-- <http://docs.aws.amazon.com/awssupport/latest/user/Welcome.html About the AWS Support API>
-- in the /AWS Support User Guide/ for information about how to use this
-- service to create and manage your support cases, and how to call Trusted
-- Advisor for results of checks on your resources.
module Network.AWS.Support
    ( module Export
    ) where

import           Network.AWS.Support.AddAttachmentsToSet                        as Export
import           Network.AWS.Support.AddCommunicationToCase                     as Export
import           Network.AWS.Support.CreateCase                                 as Export
import           Network.AWS.Support.DescribeAttachment                         as Export
import           Network.AWS.Support.DescribeCases                              as Export
import           Network.AWS.Support.DescribeCommunications                     as Export
import           Network.AWS.Support.DescribeServices                           as Export
import           Network.AWS.Support.DescribeSeverityLevels                     as Export
import           Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses as Export
import           Network.AWS.Support.DescribeTrustedAdvisorCheckResult          as Export
import           Network.AWS.Support.DescribeTrustedAdvisorChecks               as Export
import           Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries       as Export
import           Network.AWS.Support.RefreshTrustedAdvisorCheck                 as Export
import           Network.AWS.Support.ResolveCase                                as Export
import           Network.AWS.Support.Types                                      as Export
import           Network.AWS.Support.Types.Product                              as Export
import           Network.AWS.Support.Types.Sum                                  as Export
import           Network.AWS.Support.Waiters                                    as Export
