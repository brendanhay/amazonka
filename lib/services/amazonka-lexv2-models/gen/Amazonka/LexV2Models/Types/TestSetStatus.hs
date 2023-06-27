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
-- Module      : Amazonka.LexV2Models.Types.TestSetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetStatus
  ( TestSetStatus
      ( ..,
        TestSetStatus_Deleting,
        TestSetStatus_Importing,
        TestSetStatus_PendingAnnotation,
        TestSetStatus_Ready,
        TestSetStatus_ValidationError
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TestSetStatus = TestSetStatus'
  { fromTestSetStatus ::
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

pattern TestSetStatus_Deleting :: TestSetStatus
pattern TestSetStatus_Deleting = TestSetStatus' "Deleting"

pattern TestSetStatus_Importing :: TestSetStatus
pattern TestSetStatus_Importing = TestSetStatus' "Importing"

pattern TestSetStatus_PendingAnnotation :: TestSetStatus
pattern TestSetStatus_PendingAnnotation = TestSetStatus' "PendingAnnotation"

pattern TestSetStatus_Ready :: TestSetStatus
pattern TestSetStatus_Ready = TestSetStatus' "Ready"

pattern TestSetStatus_ValidationError :: TestSetStatus
pattern TestSetStatus_ValidationError = TestSetStatus' "ValidationError"

{-# COMPLETE
  TestSetStatus_Deleting,
  TestSetStatus_Importing,
  TestSetStatus_PendingAnnotation,
  TestSetStatus_Ready,
  TestSetStatus_ValidationError,
  TestSetStatus'
  #-}
