{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Macie
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-12-19@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Macie Classic
--
-- Amazon Macie Classic has been discontinued and is no longer available.
--
-- A new Amazon Macie is now available with significant design improvements
-- and additional features, at a lower price and in most Amazon Web
-- Services Regions. We encourage you to take advantage of the new and
-- improved features, and benefit from the reduced cost. To learn about
-- features and pricing for the new Macie, see
-- <http://aws.amazon.com/macie/ Amazon Macie>. To learn how to use the new
-- Macie, see the
-- <https://docs.aws.amazon.com/macie/latest/user/what-is-macie.html Amazon Macie User Guide>.
module Amazonka.Macie
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalException
    _InternalException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateMemberAccount
    AssociateMemberAccount (AssociateMemberAccount'),
    newAssociateMemberAccount,
    AssociateMemberAccountResponse (AssociateMemberAccountResponse'),
    newAssociateMemberAccountResponse,

    -- ** AssociateS3Resources
    AssociateS3Resources (AssociateS3Resources'),
    newAssociateS3Resources,
    AssociateS3ResourcesResponse (AssociateS3ResourcesResponse'),
    newAssociateS3ResourcesResponse,

    -- ** DisassociateMemberAccount
    DisassociateMemberAccount (DisassociateMemberAccount'),
    newDisassociateMemberAccount,
    DisassociateMemberAccountResponse (DisassociateMemberAccountResponse'),
    newDisassociateMemberAccountResponse,

    -- ** DisassociateS3Resources
    DisassociateS3Resources (DisassociateS3Resources'),
    newDisassociateS3Resources,
    DisassociateS3ResourcesResponse (DisassociateS3ResourcesResponse'),
    newDisassociateS3ResourcesResponse,

    -- ** ListMemberAccounts (Paginated)
    ListMemberAccounts (ListMemberAccounts'),
    newListMemberAccounts,
    ListMemberAccountsResponse (ListMemberAccountsResponse'),
    newListMemberAccountsResponse,

    -- ** ListS3Resources (Paginated)
    ListS3Resources (ListS3Resources'),
    newListS3Resources,
    ListS3ResourcesResponse (ListS3ResourcesResponse'),
    newListS3ResourcesResponse,

    -- ** UpdateS3Resources
    UpdateS3Resources (UpdateS3Resources'),
    newUpdateS3Resources,
    UpdateS3ResourcesResponse (UpdateS3ResourcesResponse'),
    newUpdateS3ResourcesResponse,

    -- * Types

    -- ** S3ContinuousClassificationType
    S3ContinuousClassificationType (..),

    -- ** S3OneTimeClassificationType
    S3OneTimeClassificationType (..),

    -- ** ClassificationType
    ClassificationType (ClassificationType'),
    newClassificationType,

    -- ** ClassificationTypeUpdate
    ClassificationTypeUpdate (ClassificationTypeUpdate'),
    newClassificationTypeUpdate,

    -- ** FailedS3Resource
    FailedS3Resource (FailedS3Resource'),
    newFailedS3Resource,

    -- ** MemberAccount
    MemberAccount (MemberAccount'),
    newMemberAccount,

    -- ** S3Resource
    S3Resource (S3Resource'),
    newS3Resource,

    -- ** S3ResourceClassification
    S3ResourceClassification (S3ResourceClassification'),
    newS3ResourceClassification,

    -- ** S3ResourceClassificationUpdate
    S3ResourceClassificationUpdate (S3ResourceClassificationUpdate'),
    newS3ResourceClassificationUpdate,
  )
where

import Amazonka.Macie.AssociateMemberAccount
import Amazonka.Macie.AssociateS3Resources
import Amazonka.Macie.DisassociateMemberAccount
import Amazonka.Macie.DisassociateS3Resources
import Amazonka.Macie.Lens
import Amazonka.Macie.ListMemberAccounts
import Amazonka.Macie.ListS3Resources
import Amazonka.Macie.Types
import Amazonka.Macie.UpdateS3Resources
import Amazonka.Macie.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Macie'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
