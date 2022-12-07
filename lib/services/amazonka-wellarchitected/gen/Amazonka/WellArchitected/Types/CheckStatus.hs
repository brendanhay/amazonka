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
-- Module      : Amazonka.WellArchitected.Types.CheckStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.CheckStatus
  ( CheckStatus
      ( ..,
        CheckStatus_ERROR,
        CheckStatus_FETCH_FAILED,
        CheckStatus_NOT_AVAILABLE,
        CheckStatus_OKAY,
        CheckStatus_WARNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CheckStatus = CheckStatus'
  { fromCheckStatus ::
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

pattern CheckStatus_ERROR :: CheckStatus
pattern CheckStatus_ERROR = CheckStatus' "ERROR"

pattern CheckStatus_FETCH_FAILED :: CheckStatus
pattern CheckStatus_FETCH_FAILED = CheckStatus' "FETCH_FAILED"

pattern CheckStatus_NOT_AVAILABLE :: CheckStatus
pattern CheckStatus_NOT_AVAILABLE = CheckStatus' "NOT_AVAILABLE"

pattern CheckStatus_OKAY :: CheckStatus
pattern CheckStatus_OKAY = CheckStatus' "OKAY"

pattern CheckStatus_WARNING :: CheckStatus
pattern CheckStatus_WARNING = CheckStatus' "WARNING"

{-# COMPLETE
  CheckStatus_ERROR,
  CheckStatus_FETCH_FAILED,
  CheckStatus_NOT_AVAILABLE,
  CheckStatus_OKAY,
  CheckStatus_WARNING,
  CheckStatus'
  #-}
