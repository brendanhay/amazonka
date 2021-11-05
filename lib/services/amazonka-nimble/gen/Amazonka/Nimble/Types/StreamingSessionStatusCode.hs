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
-- Module      : Amazonka.Nimble.Types.StreamingSessionStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingSessionStatusCode
  ( StreamingSessionStatusCode
      ( ..,
        StreamingSessionStatusCode_ACTIVE_DIRECTORY_DOMAIN_JOIN_ERROR,
        StreamingSessionStatusCode_DECRYPT_STREAMING_IMAGE_ERROR,
        StreamingSessionStatusCode_INITIALIZATION_SCRIPT_ERROR,
        StreamingSessionStatusCode_INSUFFICIENT_CAPACITY,
        StreamingSessionStatusCode_INTERNAL_ERROR,
        StreamingSessionStatusCode_NETWORK_CONNECTION_ERROR,
        StreamingSessionStatusCode_NETWORK_INTERFACE_ERROR,
        StreamingSessionStatusCode_STREAMING_SESSION_CREATE_IN_PROGRESS,
        StreamingSessionStatusCode_STREAMING_SESSION_DELETED,
        StreamingSessionStatusCode_STREAMING_SESSION_DELETE_IN_PROGRESS,
        StreamingSessionStatusCode_STREAMING_SESSION_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- |
newtype StreamingSessionStatusCode = StreamingSessionStatusCode'
  { fromStreamingSessionStatusCode ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern StreamingSessionStatusCode_ACTIVE_DIRECTORY_DOMAIN_JOIN_ERROR :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_ACTIVE_DIRECTORY_DOMAIN_JOIN_ERROR = StreamingSessionStatusCode' "ACTIVE_DIRECTORY_DOMAIN_JOIN_ERROR"

pattern StreamingSessionStatusCode_DECRYPT_STREAMING_IMAGE_ERROR :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_DECRYPT_STREAMING_IMAGE_ERROR = StreamingSessionStatusCode' "DECRYPT_STREAMING_IMAGE_ERROR"

pattern StreamingSessionStatusCode_INITIALIZATION_SCRIPT_ERROR :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_INITIALIZATION_SCRIPT_ERROR = StreamingSessionStatusCode' "INITIALIZATION_SCRIPT_ERROR"

pattern StreamingSessionStatusCode_INSUFFICIENT_CAPACITY :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_INSUFFICIENT_CAPACITY = StreamingSessionStatusCode' "INSUFFICIENT_CAPACITY"

pattern StreamingSessionStatusCode_INTERNAL_ERROR :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_INTERNAL_ERROR = StreamingSessionStatusCode' "INTERNAL_ERROR"

pattern StreamingSessionStatusCode_NETWORK_CONNECTION_ERROR :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_NETWORK_CONNECTION_ERROR = StreamingSessionStatusCode' "NETWORK_CONNECTION_ERROR"

pattern StreamingSessionStatusCode_NETWORK_INTERFACE_ERROR :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_NETWORK_INTERFACE_ERROR = StreamingSessionStatusCode' "NETWORK_INTERFACE_ERROR"

pattern StreamingSessionStatusCode_STREAMING_SESSION_CREATE_IN_PROGRESS :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_STREAMING_SESSION_CREATE_IN_PROGRESS = StreamingSessionStatusCode' "STREAMING_SESSION_CREATE_IN_PROGRESS"

pattern StreamingSessionStatusCode_STREAMING_SESSION_DELETED :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_STREAMING_SESSION_DELETED = StreamingSessionStatusCode' "STREAMING_SESSION_DELETED"

pattern StreamingSessionStatusCode_STREAMING_SESSION_DELETE_IN_PROGRESS :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_STREAMING_SESSION_DELETE_IN_PROGRESS = StreamingSessionStatusCode' "STREAMING_SESSION_DELETE_IN_PROGRESS"

pattern StreamingSessionStatusCode_STREAMING_SESSION_READY :: StreamingSessionStatusCode
pattern StreamingSessionStatusCode_STREAMING_SESSION_READY = StreamingSessionStatusCode' "STREAMING_SESSION_READY"

{-# COMPLETE
  StreamingSessionStatusCode_ACTIVE_DIRECTORY_DOMAIN_JOIN_ERROR,
  StreamingSessionStatusCode_DECRYPT_STREAMING_IMAGE_ERROR,
  StreamingSessionStatusCode_INITIALIZATION_SCRIPT_ERROR,
  StreamingSessionStatusCode_INSUFFICIENT_CAPACITY,
  StreamingSessionStatusCode_INTERNAL_ERROR,
  StreamingSessionStatusCode_NETWORK_CONNECTION_ERROR,
  StreamingSessionStatusCode_NETWORK_INTERFACE_ERROR,
  StreamingSessionStatusCode_STREAMING_SESSION_CREATE_IN_PROGRESS,
  StreamingSessionStatusCode_STREAMING_SESSION_DELETED,
  StreamingSessionStatusCode_STREAMING_SESSION_DELETE_IN_PROGRESS,
  StreamingSessionStatusCode_STREAMING_SESSION_READY,
  StreamingSessionStatusCode'
  #-}
