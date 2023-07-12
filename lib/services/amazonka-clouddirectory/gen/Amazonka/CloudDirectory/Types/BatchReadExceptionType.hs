{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudDirectory.Types.BatchReadExceptionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchReadExceptionType
  ( BatchReadExceptionType
      ( ..,
        BatchReadExceptionType_AccessDeniedException,
        BatchReadExceptionType_CannotListParentOfRootException,
        BatchReadExceptionType_DirectoryNotEnabledException,
        BatchReadExceptionType_FacetValidationException,
        BatchReadExceptionType_InternalServiceException,
        BatchReadExceptionType_InvalidArnException,
        BatchReadExceptionType_InvalidNextTokenException,
        BatchReadExceptionType_LimitExceededException,
        BatchReadExceptionType_NotIndexException,
        BatchReadExceptionType_NotNodeException,
        BatchReadExceptionType_NotPolicyException,
        BatchReadExceptionType_ResourceNotFoundException,
        BatchReadExceptionType_ValidationException
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BatchReadExceptionType = BatchReadExceptionType'
  { fromBatchReadExceptionType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern BatchReadExceptionType_AccessDeniedException :: BatchReadExceptionType
pattern BatchReadExceptionType_AccessDeniedException = BatchReadExceptionType' "AccessDeniedException"

pattern BatchReadExceptionType_CannotListParentOfRootException :: BatchReadExceptionType
pattern BatchReadExceptionType_CannotListParentOfRootException = BatchReadExceptionType' "CannotListParentOfRootException"

pattern BatchReadExceptionType_DirectoryNotEnabledException :: BatchReadExceptionType
pattern BatchReadExceptionType_DirectoryNotEnabledException = BatchReadExceptionType' "DirectoryNotEnabledException"

pattern BatchReadExceptionType_FacetValidationException :: BatchReadExceptionType
pattern BatchReadExceptionType_FacetValidationException = BatchReadExceptionType' "FacetValidationException"

pattern BatchReadExceptionType_InternalServiceException :: BatchReadExceptionType
pattern BatchReadExceptionType_InternalServiceException = BatchReadExceptionType' "InternalServiceException"

pattern BatchReadExceptionType_InvalidArnException :: BatchReadExceptionType
pattern BatchReadExceptionType_InvalidArnException = BatchReadExceptionType' "InvalidArnException"

pattern BatchReadExceptionType_InvalidNextTokenException :: BatchReadExceptionType
pattern BatchReadExceptionType_InvalidNextTokenException = BatchReadExceptionType' "InvalidNextTokenException"

pattern BatchReadExceptionType_LimitExceededException :: BatchReadExceptionType
pattern BatchReadExceptionType_LimitExceededException = BatchReadExceptionType' "LimitExceededException"

pattern BatchReadExceptionType_NotIndexException :: BatchReadExceptionType
pattern BatchReadExceptionType_NotIndexException = BatchReadExceptionType' "NotIndexException"

pattern BatchReadExceptionType_NotNodeException :: BatchReadExceptionType
pattern BatchReadExceptionType_NotNodeException = BatchReadExceptionType' "NotNodeException"

pattern BatchReadExceptionType_NotPolicyException :: BatchReadExceptionType
pattern BatchReadExceptionType_NotPolicyException = BatchReadExceptionType' "NotPolicyException"

pattern BatchReadExceptionType_ResourceNotFoundException :: BatchReadExceptionType
pattern BatchReadExceptionType_ResourceNotFoundException = BatchReadExceptionType' "ResourceNotFoundException"

pattern BatchReadExceptionType_ValidationException :: BatchReadExceptionType
pattern BatchReadExceptionType_ValidationException = BatchReadExceptionType' "ValidationException"

{-# COMPLETE
  BatchReadExceptionType_AccessDeniedException,
  BatchReadExceptionType_CannotListParentOfRootException,
  BatchReadExceptionType_DirectoryNotEnabledException,
  BatchReadExceptionType_FacetValidationException,
  BatchReadExceptionType_InternalServiceException,
  BatchReadExceptionType_InvalidArnException,
  BatchReadExceptionType_InvalidNextTokenException,
  BatchReadExceptionType_LimitExceededException,
  BatchReadExceptionType_NotIndexException,
  BatchReadExceptionType_NotNodeException,
  BatchReadExceptionType_NotPolicyException,
  BatchReadExceptionType_ResourceNotFoundException,
  BatchReadExceptionType_ValidationException,
  BatchReadExceptionType'
  #-}
