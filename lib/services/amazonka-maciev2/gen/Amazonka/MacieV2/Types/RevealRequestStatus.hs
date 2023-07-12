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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of a request to retrieve occurrences of sensitive data
-- reported by a finding. Possible values are:
newtype RevealRequestStatus = RevealRequestStatus'
  { fromRevealRequestStatus ::
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
