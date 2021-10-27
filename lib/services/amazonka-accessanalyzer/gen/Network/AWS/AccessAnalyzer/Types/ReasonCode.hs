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
-- Module      : Network.AWS.AccessAnalyzer.Types.ReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.ReasonCode
  ( ReasonCode
      ( ..,
        ReasonCode_AWS_SERVICE_ACCESS_DISABLED,
        ReasonCode_DELEGATED_ADMINISTRATOR_DEREGISTERED,
        ReasonCode_ORGANIZATION_DELETED,
        ReasonCode_SERVICE_LINKED_ROLE_CREATION_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ReasonCode = ReasonCode'
  { fromReasonCode ::
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

pattern ReasonCode_AWS_SERVICE_ACCESS_DISABLED :: ReasonCode
pattern ReasonCode_AWS_SERVICE_ACCESS_DISABLED = ReasonCode' "AWS_SERVICE_ACCESS_DISABLED"

pattern ReasonCode_DELEGATED_ADMINISTRATOR_DEREGISTERED :: ReasonCode
pattern ReasonCode_DELEGATED_ADMINISTRATOR_DEREGISTERED = ReasonCode' "DELEGATED_ADMINISTRATOR_DEREGISTERED"

pattern ReasonCode_ORGANIZATION_DELETED :: ReasonCode
pattern ReasonCode_ORGANIZATION_DELETED = ReasonCode' "ORGANIZATION_DELETED"

pattern ReasonCode_SERVICE_LINKED_ROLE_CREATION_FAILED :: ReasonCode
pattern ReasonCode_SERVICE_LINKED_ROLE_CREATION_FAILED = ReasonCode' "SERVICE_LINKED_ROLE_CREATION_FAILED"

{-# COMPLETE
  ReasonCode_AWS_SERVICE_ACCESS_DISABLED,
  ReasonCode_DELEGATED_ADMINISTRATOR_DEREGISTERED,
  ReasonCode_ORGANIZATION_DELETED,
  ReasonCode_SERVICE_LINKED_ROLE_CREATION_FAILED,
  ReasonCode'
  #-}
