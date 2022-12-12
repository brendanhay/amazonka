{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Macie.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Lens
  ( -- * Operations

    -- ** AssociateMemberAccount
    associateMemberAccount_memberAccountId,

    -- ** AssociateS3Resources
    associateS3Resources_memberAccountId,
    associateS3Resources_s3Resources,
    associateS3ResourcesResponse_failedS3Resources,
    associateS3ResourcesResponse_httpStatus,

    -- ** DisassociateMemberAccount
    disassociateMemberAccount_memberAccountId,

    -- ** DisassociateS3Resources
    disassociateS3Resources_memberAccountId,
    disassociateS3Resources_associatedS3Resources,
    disassociateS3ResourcesResponse_failedS3Resources,
    disassociateS3ResourcesResponse_httpStatus,

    -- ** ListMemberAccounts
    listMemberAccounts_maxResults,
    listMemberAccounts_nextToken,
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_httpStatus,

    -- ** ListS3Resources
    listS3Resources_maxResults,
    listS3Resources_memberAccountId,
    listS3Resources_nextToken,
    listS3ResourcesResponse_nextToken,
    listS3ResourcesResponse_s3Resources,
    listS3ResourcesResponse_httpStatus,

    -- ** UpdateS3Resources
    updateS3Resources_memberAccountId,
    updateS3Resources_s3ResourcesUpdate,
    updateS3ResourcesResponse_failedS3Resources,
    updateS3ResourcesResponse_httpStatus,

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

import Amazonka.Macie.AssociateMemberAccount
import Amazonka.Macie.AssociateS3Resources
import Amazonka.Macie.DisassociateMemberAccount
import Amazonka.Macie.DisassociateS3Resources
import Amazonka.Macie.ListMemberAccounts
import Amazonka.Macie.ListS3Resources
import Amazonka.Macie.Types.ClassificationType
import Amazonka.Macie.Types.ClassificationTypeUpdate
import Amazonka.Macie.Types.FailedS3Resource
import Amazonka.Macie.Types.MemberAccount
import Amazonka.Macie.Types.S3Resource
import Amazonka.Macie.Types.S3ResourceClassification
import Amazonka.Macie.Types.S3ResourceClassificationUpdate
import Amazonka.Macie.UpdateS3Resources
