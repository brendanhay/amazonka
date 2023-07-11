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
-- Module      : Amazonka.SageMaker.Types.DetailedAlgorithmStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DetailedAlgorithmStatus
  ( DetailedAlgorithmStatus
      ( ..,
        DetailedAlgorithmStatus_Completed,
        DetailedAlgorithmStatus_Failed,
        DetailedAlgorithmStatus_InProgress,
        DetailedAlgorithmStatus_NotStarted
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DetailedAlgorithmStatus = DetailedAlgorithmStatus'
  { fromDetailedAlgorithmStatus ::
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

pattern DetailedAlgorithmStatus_Completed :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_Completed = DetailedAlgorithmStatus' "Completed"

pattern DetailedAlgorithmStatus_Failed :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_Failed = DetailedAlgorithmStatus' "Failed"

pattern DetailedAlgorithmStatus_InProgress :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_InProgress = DetailedAlgorithmStatus' "InProgress"

pattern DetailedAlgorithmStatus_NotStarted :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_NotStarted = DetailedAlgorithmStatus' "NotStarted"

{-# COMPLETE
  DetailedAlgorithmStatus_Completed,
  DetailedAlgorithmStatus_Failed,
  DetailedAlgorithmStatus_InProgress,
  DetailedAlgorithmStatus_NotStarted,
  DetailedAlgorithmStatus'
  #-}
