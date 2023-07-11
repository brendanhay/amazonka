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
-- Module      : Amazonka.SSOAdmin.Types.StatusValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.StatusValues
  ( StatusValues
      ( ..,
        StatusValues_FAILED,
        StatusValues_IN_PROGRESS,
        StatusValues_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StatusValues = StatusValues'
  { fromStatusValues ::
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

pattern StatusValues_FAILED :: StatusValues
pattern StatusValues_FAILED = StatusValues' "FAILED"

pattern StatusValues_IN_PROGRESS :: StatusValues
pattern StatusValues_IN_PROGRESS = StatusValues' "IN_PROGRESS"

pattern StatusValues_SUCCEEDED :: StatusValues
pattern StatusValues_SUCCEEDED = StatusValues' "SUCCEEDED"

{-# COMPLETE
  StatusValues_FAILED,
  StatusValues_IN_PROGRESS,
  StatusValues_SUCCEEDED,
  StatusValues'
  #-}
