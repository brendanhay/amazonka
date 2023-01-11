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
-- Module      : Amazonka.CloudFormation.Types.TypeTestsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.TypeTestsStatus
  ( TypeTestsStatus
      ( ..,
        TypeTestsStatus_FAILED,
        TypeTestsStatus_IN_PROGRESS,
        TypeTestsStatus_NOT_TESTED,
        TypeTestsStatus_PASSED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TypeTestsStatus = TypeTestsStatus'
  { fromTypeTestsStatus ::
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

pattern TypeTestsStatus_FAILED :: TypeTestsStatus
pattern TypeTestsStatus_FAILED = TypeTestsStatus' "FAILED"

pattern TypeTestsStatus_IN_PROGRESS :: TypeTestsStatus
pattern TypeTestsStatus_IN_PROGRESS = TypeTestsStatus' "IN_PROGRESS"

pattern TypeTestsStatus_NOT_TESTED :: TypeTestsStatus
pattern TypeTestsStatus_NOT_TESTED = TypeTestsStatus' "NOT_TESTED"

pattern TypeTestsStatus_PASSED :: TypeTestsStatus
pattern TypeTestsStatus_PASSED = TypeTestsStatus' "PASSED"

{-# COMPLETE
  TypeTestsStatus_FAILED,
  TypeTestsStatus_IN_PROGRESS,
  TypeTestsStatus_NOT_TESTED,
  TypeTestsStatus_PASSED,
  TypeTestsStatus'
  #-}
