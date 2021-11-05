{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Macie.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Macie.Lens
  ( -- * Operations

    -- ** AssociateS3Resources
    associateS3Resources_memberAccountId,
    associateS3Resources_s3Resources,
    associateS3ResourcesResponse_failedS3Resources,
    associateS3ResourcesResponse_httpStatus,

    -- ** AssociateMemberAccount
    associateMemberAccount_memberAccountId,

    -- ** UpdateS3Resources
    updateS3Resources_memberAccountId,
    updateS3Resources_s3ResourcesUpdate,
    updateS3ResourcesResponse_failedS3Resources,
    updateS3ResourcesResponse_httpStatus,

    -- ** ListMemberAccounts
    listMemberAccounts_nextToken,
    listMemberAccounts_maxResults,
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_httpStatus,

    -- ** DisassociateMemberAccount
    disassociateMemberAccount_memberAccountId,

    -- ** ListS3Resources
    listS3Resources_memberAccountId,
    listS3Resources_nextToken,
    listS3Resources_maxResults,
    listS3ResourcesResponse_nextToken,
    listS3ResourcesResponse_s3Resources,
    listS3ResourcesResponse_httpStatus,

    -- ** DisassociateS3Resources
    disassociateS3Resources_memberAccountId,
    disassociateS3Resources_associatedS3Resources,
    disassociateS3ResourcesResponse_failedS3Resources,
    disassociateS3ResourcesResponse_httpStatus,

    -- * Types

    -- ** ClassificationType
    classificationType_oneTime,
    classificationType_continuous,

    -- ** ClassificationTypeUpdate
    classificationTypeUpdate_continuous,
    classificationTypeUpdate_oneTime,

    -- ** FailedS3Resource
    failedS3Resource_errorCode,
    failedS3Resource_errorMessage,
    failedS3Resource_failedItem,

    -- ** MemberAccount
    memberAccount_accountId,

    -- ** S3Resource
    s3Resource_prefix,
    s3Resource_bucketName,

    -- ** S3ResourceClassification
    s3ResourceClassification_prefix,
    s3ResourceClassification_bucketName,
    s3ResourceClassification_classificationType,

    -- ** S3ResourceClassificationUpdate
    s3ResourceClassificationUpdate_prefix,
    s3ResourceClassificationUpdate_bucketName,
    s3ResourceClassificationUpdate_classificationTypeUpdate,
  )
where

import Network.AWS.Macie.AssociateMemberAccount
import Network.AWS.Macie.AssociateS3Resources
import Network.AWS.Macie.DisassociateMemberAccount
import Network.AWS.Macie.DisassociateS3Resources
import Network.AWS.Macie.ListMemberAccounts
import Network.AWS.Macie.ListS3Resources
import Network.AWS.Macie.Types.ClassificationType
import Network.AWS.Macie.Types.ClassificationTypeUpdate
import Network.AWS.Macie.Types.FailedS3Resource
import Network.AWS.Macie.Types.MemberAccount
import Network.AWS.Macie.Types.S3Resource
import Network.AWS.Macie.Types.S3ResourceClassification
import Network.AWS.Macie.Types.S3ResourceClassificationUpdate
import Network.AWS.Macie.UpdateS3Resources
