{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadExceptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchReadExceptionType
  ( BatchReadExceptionType
    ( BatchReadExceptionType'
    , BatchReadExceptionTypeValidationException
    , BatchReadExceptionTypeInvalidArnException
    , BatchReadExceptionTypeResourceNotFoundException
    , BatchReadExceptionTypeInvalidNextTokenException
    , BatchReadExceptionTypeAccessDeniedException
    , BatchReadExceptionTypeNotNodeException
    , BatchReadExceptionTypeFacetValidationException
    , BatchReadExceptionTypeCannotListParentOfRootException
    , BatchReadExceptionTypeNotIndexException
    , BatchReadExceptionTypeNotPolicyException
    , BatchReadExceptionTypeDirectoryNotEnabledException
    , BatchReadExceptionTypeLimitExceededException
    , BatchReadExceptionTypeInternalServiceException
    , fromBatchReadExceptionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BatchReadExceptionType = BatchReadExceptionType'{fromBatchReadExceptionType
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern BatchReadExceptionTypeValidationException :: BatchReadExceptionType
pattern BatchReadExceptionTypeValidationException = BatchReadExceptionType' "ValidationException"

pattern BatchReadExceptionTypeInvalidArnException :: BatchReadExceptionType
pattern BatchReadExceptionTypeInvalidArnException = BatchReadExceptionType' "InvalidArnException"

pattern BatchReadExceptionTypeResourceNotFoundException :: BatchReadExceptionType
pattern BatchReadExceptionTypeResourceNotFoundException = BatchReadExceptionType' "ResourceNotFoundException"

pattern BatchReadExceptionTypeInvalidNextTokenException :: BatchReadExceptionType
pattern BatchReadExceptionTypeInvalidNextTokenException = BatchReadExceptionType' "InvalidNextTokenException"

pattern BatchReadExceptionTypeAccessDeniedException :: BatchReadExceptionType
pattern BatchReadExceptionTypeAccessDeniedException = BatchReadExceptionType' "AccessDeniedException"

pattern BatchReadExceptionTypeNotNodeException :: BatchReadExceptionType
pattern BatchReadExceptionTypeNotNodeException = BatchReadExceptionType' "NotNodeException"

pattern BatchReadExceptionTypeFacetValidationException :: BatchReadExceptionType
pattern BatchReadExceptionTypeFacetValidationException = BatchReadExceptionType' "FacetValidationException"

pattern BatchReadExceptionTypeCannotListParentOfRootException :: BatchReadExceptionType
pattern BatchReadExceptionTypeCannotListParentOfRootException = BatchReadExceptionType' "CannotListParentOfRootException"

pattern BatchReadExceptionTypeNotIndexException :: BatchReadExceptionType
pattern BatchReadExceptionTypeNotIndexException = BatchReadExceptionType' "NotIndexException"

pattern BatchReadExceptionTypeNotPolicyException :: BatchReadExceptionType
pattern BatchReadExceptionTypeNotPolicyException = BatchReadExceptionType' "NotPolicyException"

pattern BatchReadExceptionTypeDirectoryNotEnabledException :: BatchReadExceptionType
pattern BatchReadExceptionTypeDirectoryNotEnabledException = BatchReadExceptionType' "DirectoryNotEnabledException"

pattern BatchReadExceptionTypeLimitExceededException :: BatchReadExceptionType
pattern BatchReadExceptionTypeLimitExceededException = BatchReadExceptionType' "LimitExceededException"

pattern BatchReadExceptionTypeInternalServiceException :: BatchReadExceptionType
pattern BatchReadExceptionTypeInternalServiceException = BatchReadExceptionType' "InternalServiceException"

{-# COMPLETE 
  BatchReadExceptionTypeValidationException,

  BatchReadExceptionTypeInvalidArnException,

  BatchReadExceptionTypeResourceNotFoundException,

  BatchReadExceptionTypeInvalidNextTokenException,

  BatchReadExceptionTypeAccessDeniedException,

  BatchReadExceptionTypeNotNodeException,

  BatchReadExceptionTypeFacetValidationException,

  BatchReadExceptionTypeCannotListParentOfRootException,

  BatchReadExceptionTypeNotIndexException,

  BatchReadExceptionTypeNotPolicyException,

  BatchReadExceptionTypeDirectoryNotEnabledException,

  BatchReadExceptionTypeLimitExceededException,

  BatchReadExceptionTypeInternalServiceException,
  BatchReadExceptionType'
  #-}
