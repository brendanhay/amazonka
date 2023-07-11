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
-- Module      : Amazonka.AppMesh.Types.GrpcRetryPolicyEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcRetryPolicyEvent
  ( GrpcRetryPolicyEvent
      ( ..,
        GrpcRetryPolicyEvent_Cancelled,
        GrpcRetryPolicyEvent_Deadline_exceeded,
        GrpcRetryPolicyEvent_Internal,
        GrpcRetryPolicyEvent_Resource_exhausted,
        GrpcRetryPolicyEvent_Unavailable
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GrpcRetryPolicyEvent = GrpcRetryPolicyEvent'
  { fromGrpcRetryPolicyEvent ::
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

pattern GrpcRetryPolicyEvent_Cancelled :: GrpcRetryPolicyEvent
pattern GrpcRetryPolicyEvent_Cancelled = GrpcRetryPolicyEvent' "cancelled"

pattern GrpcRetryPolicyEvent_Deadline_exceeded :: GrpcRetryPolicyEvent
pattern GrpcRetryPolicyEvent_Deadline_exceeded = GrpcRetryPolicyEvent' "deadline-exceeded"

pattern GrpcRetryPolicyEvent_Internal :: GrpcRetryPolicyEvent
pattern GrpcRetryPolicyEvent_Internal = GrpcRetryPolicyEvent' "internal"

pattern GrpcRetryPolicyEvent_Resource_exhausted :: GrpcRetryPolicyEvent
pattern GrpcRetryPolicyEvent_Resource_exhausted = GrpcRetryPolicyEvent' "resource-exhausted"

pattern GrpcRetryPolicyEvent_Unavailable :: GrpcRetryPolicyEvent
pattern GrpcRetryPolicyEvent_Unavailable = GrpcRetryPolicyEvent' "unavailable"

{-# COMPLETE
  GrpcRetryPolicyEvent_Cancelled,
  GrpcRetryPolicyEvent_Deadline_exceeded,
  GrpcRetryPolicyEvent_Internal,
  GrpcRetryPolicyEvent_Resource_exhausted,
  GrpcRetryPolicyEvent_Unavailable,
  GrpcRetryPolicyEvent'
  #-}
