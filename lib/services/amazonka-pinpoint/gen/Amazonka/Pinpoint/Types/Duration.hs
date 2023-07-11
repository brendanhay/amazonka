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
-- Module      : Amazonka.Pinpoint.Types.Duration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.Duration
  ( Duration
      ( ..,
        Duration_DAY_14,
        Duration_DAY_30,
        Duration_DAY_7,
        Duration_HR_24
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Duration = Duration'
  { fromDuration ::
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

pattern Duration_DAY_14 :: Duration
pattern Duration_DAY_14 = Duration' "DAY_14"

pattern Duration_DAY_30 :: Duration
pattern Duration_DAY_30 = Duration' "DAY_30"

pattern Duration_DAY_7 :: Duration
pattern Duration_DAY_7 = Duration' "DAY_7"

pattern Duration_HR_24 :: Duration
pattern Duration_HR_24 = Duration' "HR_24"

{-# COMPLETE
  Duration_DAY_14,
  Duration_DAY_30,
  Duration_DAY_7,
  Duration_HR_24,
  Duration'
  #-}
