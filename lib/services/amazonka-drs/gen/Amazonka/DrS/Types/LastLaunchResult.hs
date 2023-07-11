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
-- Module      : Amazonka.DrS.Types.LastLaunchResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.LastLaunchResult
  ( LastLaunchResult
      ( ..,
        LastLaunchResult_FAILED,
        LastLaunchResult_NOT_STARTED,
        LastLaunchResult_PENDING,
        LastLaunchResult_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LastLaunchResult = LastLaunchResult'
  { fromLastLaunchResult ::
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

pattern LastLaunchResult_FAILED :: LastLaunchResult
pattern LastLaunchResult_FAILED = LastLaunchResult' "FAILED"

pattern LastLaunchResult_NOT_STARTED :: LastLaunchResult
pattern LastLaunchResult_NOT_STARTED = LastLaunchResult' "NOT_STARTED"

pattern LastLaunchResult_PENDING :: LastLaunchResult
pattern LastLaunchResult_PENDING = LastLaunchResult' "PENDING"

pattern LastLaunchResult_SUCCEEDED :: LastLaunchResult
pattern LastLaunchResult_SUCCEEDED = LastLaunchResult' "SUCCEEDED"

{-# COMPLETE
  LastLaunchResult_FAILED,
  LastLaunchResult_NOT_STARTED,
  LastLaunchResult_PENDING,
  LastLaunchResult_SUCCEEDED,
  LastLaunchResult'
  #-}
