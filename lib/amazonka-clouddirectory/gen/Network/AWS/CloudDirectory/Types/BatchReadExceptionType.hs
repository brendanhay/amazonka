{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ValidationException,
        InvalidARNException,
        ResourceNotFoundException,
        InvalidNextTokenException,
        AccessDeniedException,
        NotNodeException,
        FacetValidationException,
        CannotListParentOfRootException,
        NotIndexException,
        NotPolicyException,
        DirectoryNotEnabledException,
        LimitExceededException,
        InternalServiceException
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

pattern ValidationException :: BatchReadExceptionType
pattern ValidationException = BatchReadExceptionType' "ValidationException"

pattern InvalidARNException :: BatchReadExceptionType
pattern InvalidARNException = BatchReadExceptionType' "InvalidArnException"

pattern ResourceNotFoundException :: BatchReadExceptionType
pattern ResourceNotFoundException = BatchReadExceptionType' "ResourceNotFoundException"

pattern InvalidNextTokenException :: BatchReadExceptionType
pattern InvalidNextTokenException = BatchReadExceptionType' "InvalidNextTokenException"

pattern AccessDeniedException :: BatchReadExceptionType
pattern AccessDeniedException = BatchReadExceptionType' "AccessDeniedException"

pattern NotNodeException :: BatchReadExceptionType
pattern NotNodeException = BatchReadExceptionType' "NotNodeException"

pattern FacetValidationException :: BatchReadExceptionType
pattern FacetValidationException = BatchReadExceptionType' "FacetValidationException"

pattern CannotListParentOfRootException :: BatchReadExceptionType
pattern CannotListParentOfRootException = BatchReadExceptionType' "CannotListParentOfRootException"

pattern NotIndexException :: BatchReadExceptionType
pattern NotIndexException = BatchReadExceptionType' "NotIndexException"

pattern NotPolicyException :: BatchReadExceptionType
pattern NotPolicyException = BatchReadExceptionType' "NotPolicyException"

pattern DirectoryNotEnabledException :: BatchReadExceptionType
pattern DirectoryNotEnabledException = BatchReadExceptionType' "DirectoryNotEnabledException"

pattern LimitExceededException :: BatchReadExceptionType
pattern LimitExceededException = BatchReadExceptionType' "LimitExceededException"

pattern InternalServiceException :: BatchReadExceptionType
pattern InternalServiceException = BatchReadExceptionType' "InternalServiceException"

{-# COMPLETE
  ValidationException,
  InvalidARNException,
  ResourceNotFoundException,
  InvalidNextTokenException,
  AccessDeniedException,
  NotNodeException,
  FacetValidationException,
  CannotListParentOfRootException,
  NotIndexException,
  NotPolicyException,
  DirectoryNotEnabledException,
  LimitExceededException,
  InternalServiceException,
  BatchReadExceptionType'
  #-}
