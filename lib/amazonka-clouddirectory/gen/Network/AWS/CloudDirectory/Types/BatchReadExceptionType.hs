{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadExceptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadExceptionType where

import Network.AWS.Prelude

data BatchReadExceptionType
  = AccessDeniedException
  | CannotListParentOfRootException
  | DirectoryNotEnabledException
  | FacetValidationException
  | InternalServiceException
  | InvalidARNException
  | InvalidNextTokenException
  | LimitExceededException
  | NotIndexException
  | NotNodeException
  | NotPolicyException
  | ResourceNotFoundException
  | ValidationException
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText BatchReadExceptionType where
  parser =
    takeLowerText >>= \case
      "accessdeniedexception" -> pure AccessDeniedException
      "cannotlistparentofrootexception" -> pure CannotListParentOfRootException
      "directorynotenabledexception" -> pure DirectoryNotEnabledException
      "facetvalidationexception" -> pure FacetValidationException
      "internalserviceexception" -> pure InternalServiceException
      "invalidarnexception" -> pure InvalidARNException
      "invalidnexttokenexception" -> pure InvalidNextTokenException
      "limitexceededexception" -> pure LimitExceededException
      "notindexexception" -> pure NotIndexException
      "notnodeexception" -> pure NotNodeException
      "notpolicyexception" -> pure NotPolicyException
      "resourcenotfoundexception" -> pure ResourceNotFoundException
      "validationexception" -> pure ValidationException
      e ->
        fromTextError $
          "Failure parsing BatchReadExceptionType from value: '" <> e
            <> "'. Accepted values: accessdeniedexception, cannotlistparentofrootexception, directorynotenabledexception, facetvalidationexception, internalserviceexception, invalidarnexception, invalidnexttokenexception, limitexceededexception, notindexexception, notnodeexception, notpolicyexception, resourcenotfoundexception, validationexception"

instance ToText BatchReadExceptionType where
  toText = \case
    AccessDeniedException -> "AccessDeniedException"
    CannotListParentOfRootException -> "CannotListParentOfRootException"
    DirectoryNotEnabledException -> "DirectoryNotEnabledException"
    FacetValidationException -> "FacetValidationException"
    InternalServiceException -> "InternalServiceException"
    InvalidARNException -> "InvalidArnException"
    InvalidNextTokenException -> "InvalidNextTokenException"
    LimitExceededException -> "LimitExceededException"
    NotIndexException -> "NotIndexException"
    NotNodeException -> "NotNodeException"
    NotPolicyException -> "NotPolicyException"
    ResourceNotFoundException -> "ResourceNotFoundException"
    ValidationException -> "ValidationException"

instance Hashable BatchReadExceptionType

instance NFData BatchReadExceptionType

instance ToByteString BatchReadExceptionType

instance ToQuery BatchReadExceptionType

instance ToHeader BatchReadExceptionType

instance FromJSON BatchReadExceptionType where
  parseJSON = parseJSONText "BatchReadExceptionType"
