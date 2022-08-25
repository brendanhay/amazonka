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
-- Module      : Amazonka.MacieV2.Types.RevealRequestStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.RevealRequestStatus
  ( RevealRequestStatus
      ( ..,
        RevealRequestStatus_ERROR,
        RevealRequestStatus_PROCESSING,
        RevealRequestStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The status of a request to retrieve occurrences of sensitive data
-- reported by a finding. Possible values are:
newtype RevealRequestStatus = RevealRequestStatus'
  { fromRevealRequestStatus ::
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

pattern RevealRequestStatus_ERROR :: RevealRequestStatus
pattern RevealRequestStatus_ERROR = RevealRequestStatus' "ERROR"

pattern RevealRequestStatus_PROCESSING :: RevealRequestStatus
pattern RevealRequestStatus_PROCESSING = RevealRequestStatus' "PROCESSING"

pattern RevealRequestStatus_SUCCESS :: RevealRequestStatus
pattern RevealRequestStatus_SUCCESS = RevealRequestStatus' "SUCCESS"

{-# COMPLETE
  RevealRequestStatus_ERROR,
  RevealRequestStatus_PROCESSING,
  RevealRequestStatus_SUCCESS,
  RevealRequestStatus'
  #-}
