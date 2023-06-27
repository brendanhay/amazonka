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
-- Module      : Amazonka.LexV2Models.Types.TestSetGenerationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetGenerationStatus
  ( TestSetGenerationStatus
      ( ..,
        TestSetGenerationStatus_Failed,
        TestSetGenerationStatus_Generating,
        TestSetGenerationStatus_Pending,
        TestSetGenerationStatus_Ready
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TestSetGenerationStatus = TestSetGenerationStatus'
  { fromTestSetGenerationStatus ::
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

pattern TestSetGenerationStatus_Failed :: TestSetGenerationStatus
pattern TestSetGenerationStatus_Failed = TestSetGenerationStatus' "Failed"

pattern TestSetGenerationStatus_Generating :: TestSetGenerationStatus
pattern TestSetGenerationStatus_Generating = TestSetGenerationStatus' "Generating"

pattern TestSetGenerationStatus_Pending :: TestSetGenerationStatus
pattern TestSetGenerationStatus_Pending = TestSetGenerationStatus' "Pending"

pattern TestSetGenerationStatus_Ready :: TestSetGenerationStatus
pattern TestSetGenerationStatus_Ready = TestSetGenerationStatus' "Ready"

{-# COMPLETE
  TestSetGenerationStatus_Failed,
  TestSetGenerationStatus_Generating,
  TestSetGenerationStatus_Pending,
  TestSetGenerationStatus_Ready,
  TestSetGenerationStatus'
  #-}
