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
-- Module      : Amazonka.DataSync.Types.VerifyMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.VerifyMode
  ( VerifyMode
      ( ..,
        VerifyMode_NONE,
        VerifyMode_ONLY_FILES_TRANSFERRED,
        VerifyMode_POINT_IN_TIME_CONSISTENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VerifyMode = VerifyMode'
  { fromVerifyMode ::
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

pattern VerifyMode_NONE :: VerifyMode
pattern VerifyMode_NONE = VerifyMode' "NONE"

pattern VerifyMode_ONLY_FILES_TRANSFERRED :: VerifyMode
pattern VerifyMode_ONLY_FILES_TRANSFERRED = VerifyMode' "ONLY_FILES_TRANSFERRED"

pattern VerifyMode_POINT_IN_TIME_CONSISTENT :: VerifyMode
pattern VerifyMode_POINT_IN_TIME_CONSISTENT = VerifyMode' "POINT_IN_TIME_CONSISTENT"

{-# COMPLETE
  VerifyMode_NONE,
  VerifyMode_ONLY_FILES_TRANSFERRED,
  VerifyMode_POINT_IN_TIME_CONSISTENT,
  VerifyMode'
  #-}
