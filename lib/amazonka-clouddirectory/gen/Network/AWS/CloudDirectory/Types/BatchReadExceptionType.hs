-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadExceptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadExceptionType
  ( BatchReadExceptionType
      ( BatchReadExceptionType',
        AccessDeniedException,
        CannotListParentOfRootException,
        DirectoryNotEnabledException,
        FacetValidationException,
        InternalServiceException,
        InvalidARNException,
        InvalidNextTokenException,
        LimitExceededException,
        NotIndexException,
        NotNodeException,
        NotPolicyException,
        ResourceNotFoundException,
        ValidationException
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BatchReadExceptionType = BatchReadExceptionType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AccessDeniedException :: BatchReadExceptionType
pattern AccessDeniedException = BatchReadExceptionType' "AccessDeniedException"

pattern CannotListParentOfRootException :: BatchReadExceptionType
pattern CannotListParentOfRootException = BatchReadExceptionType' "CannotListParentOfRootException"

pattern DirectoryNotEnabledException :: BatchReadExceptionType
pattern DirectoryNotEnabledException = BatchReadExceptionType' "DirectoryNotEnabledException"

pattern FacetValidationException :: BatchReadExceptionType
pattern FacetValidationException = BatchReadExceptionType' "FacetValidationException"

pattern InternalServiceException :: BatchReadExceptionType
pattern InternalServiceException = BatchReadExceptionType' "InternalServiceException"

pattern InvalidARNException :: BatchReadExceptionType
pattern InvalidARNException = BatchReadExceptionType' "InvalidArnException"

pattern InvalidNextTokenException :: BatchReadExceptionType
pattern InvalidNextTokenException = BatchReadExceptionType' "InvalidNextTokenException"

pattern LimitExceededException :: BatchReadExceptionType
pattern LimitExceededException = BatchReadExceptionType' "LimitExceededException"

pattern NotIndexException :: BatchReadExceptionType
pattern NotIndexException = BatchReadExceptionType' "NotIndexException"

pattern NotNodeException :: BatchReadExceptionType
pattern NotNodeException = BatchReadExceptionType' "NotNodeException"

pattern NotPolicyException :: BatchReadExceptionType
pattern NotPolicyException = BatchReadExceptionType' "NotPolicyException"

pattern ResourceNotFoundException :: BatchReadExceptionType
pattern ResourceNotFoundException = BatchReadExceptionType' "ResourceNotFoundException"

pattern ValidationException :: BatchReadExceptionType
pattern ValidationException = BatchReadExceptionType' "ValidationException"

{-# COMPLETE
  AccessDeniedException,
  CannotListParentOfRootException,
  DirectoryNotEnabledException,
  FacetValidationException,
  InternalServiceException,
  InvalidARNException,
  InvalidNextTokenException,
  LimitExceededException,
  NotIndexException,
  NotNodeException,
  NotPolicyException,
  ResourceNotFoundException,
  ValidationException,
  BatchReadExceptionType'
  #-}
