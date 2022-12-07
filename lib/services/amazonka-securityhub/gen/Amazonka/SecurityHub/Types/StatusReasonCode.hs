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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StatusReasonCode = StatusReasonCode'
  { fromStatusReasonCode ::
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

pattern StatusReasonCode_INTERNAL_ERROR :: StatusReasonCode
pattern StatusReasonCode_INTERNAL_ERROR = StatusReasonCode' "INTERNAL_ERROR"

pattern StatusReasonCode_NO_AVAILABLE_CONFIGURATION_RECORDER :: StatusReasonCode
pattern StatusReasonCode_NO_AVAILABLE_CONFIGURATION_RECORDER = StatusReasonCode' "NO_AVAILABLE_CONFIGURATION_RECORDER"

{-# COMPLETE
  StatusReasonCode_INTERNAL_ERROR,
  StatusReasonCode_NO_AVAILABLE_CONFIGURATION_RECORDER,
  StatusReasonCode'
  #-}
