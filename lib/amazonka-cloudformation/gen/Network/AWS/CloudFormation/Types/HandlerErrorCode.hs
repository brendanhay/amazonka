{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.HandlerErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.HandlerErrorCode where

import Network.AWS.Prelude

data HandlerErrorCode
  = AccessDenied
  | AlreadyExists
  | GeneralServiceException
  | InternalFailure
  | InvalidCredentials
  | InvalidRequest
  | NetworkFailure
  | NotFound
  | NotStabilized
  | NotUpdatable
  | ResourceConflict
  | ServiceInternalError
  | ServiceLimitExceeded
  | Throttling
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

instance FromText HandlerErrorCode where
  parser =
    takeLowerText >>= \case
      "accessdenied" -> pure AccessDenied
      "alreadyexists" -> pure AlreadyExists
      "generalserviceexception" -> pure GeneralServiceException
      "internalfailure" -> pure InternalFailure
      "invalidcredentials" -> pure InvalidCredentials
      "invalidrequest" -> pure InvalidRequest
      "networkfailure" -> pure NetworkFailure
      "notfound" -> pure NotFound
      "notstabilized" -> pure NotStabilized
      "notupdatable" -> pure NotUpdatable
      "resourceconflict" -> pure ResourceConflict
      "serviceinternalerror" -> pure ServiceInternalError
      "servicelimitexceeded" -> pure ServiceLimitExceeded
      "throttling" -> pure Throttling
      e ->
        fromTextError $
          "Failure parsing HandlerErrorCode from value: '" <> e
            <> "'. Accepted values: accessdenied, alreadyexists, generalserviceexception, internalfailure, invalidcredentials, invalidrequest, networkfailure, notfound, notstabilized, notupdatable, resourceconflict, serviceinternalerror, servicelimitexceeded, throttling"

instance ToText HandlerErrorCode where
  toText = \case
    AccessDenied -> "AccessDenied"
    AlreadyExists -> "AlreadyExists"
    GeneralServiceException -> "GeneralServiceException"
    InternalFailure -> "InternalFailure"
    InvalidCredentials -> "InvalidCredentials"
    InvalidRequest -> "InvalidRequest"
    NetworkFailure -> "NetworkFailure"
    NotFound -> "NotFound"
    NotStabilized -> "NotStabilized"
    NotUpdatable -> "NotUpdatable"
    ResourceConflict -> "ResourceConflict"
    ServiceInternalError -> "ServiceInternalError"
    ServiceLimitExceeded -> "ServiceLimitExceeded"
    Throttling -> "Throttling"

instance Hashable HandlerErrorCode

instance NFData HandlerErrorCode

instance ToByteString HandlerErrorCode

instance ToQuery HandlerErrorCode

instance ToHeader HandlerErrorCode
