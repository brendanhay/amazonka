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
-- Module      : Amazonka.SecurityHub.Types.StatusReasonCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StatusReasonCode
  ( StatusReasonCode
      ( ..,
        StatusReasonCode_INTERNAL_ERROR,
        StatusReasonCode_NO_AVAILABLE_CONFIGURATION_RECORDER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StatusReasonCode = StatusReasonCode'
  { fromStatusReasonCode ::
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

pattern StatusReasonCode_INTERNAL_ERROR :: StatusReasonCode
pattern StatusReasonCode_INTERNAL_ERROR = StatusReasonCode' "INTERNAL_ERROR"

pattern StatusReasonCode_NO_AVAILABLE_CONFIGURATION_RECORDER :: StatusReasonCode
pattern StatusReasonCode_NO_AVAILABLE_CONFIGURATION_RECORDER = StatusReasonCode' "NO_AVAILABLE_CONFIGURATION_RECORDER"

{-# COMPLETE
  StatusReasonCode_INTERNAL_ERROR,
  StatusReasonCode_NO_AVAILABLE_CONFIGURATION_RECORDER,
  StatusReasonCode'
  #-}
