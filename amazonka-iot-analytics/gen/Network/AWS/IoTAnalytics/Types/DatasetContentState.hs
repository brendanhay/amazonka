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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentState
  ( DatasetContentState
      ( ..,
        DatasetContentState_CREATING,
        DatasetContentState_FAILED,
        DatasetContentState_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DatasetContentState = DatasetContentState'
  { fromDatasetContentState ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DatasetContentState_CREATING :: DatasetContentState
pattern DatasetContentState_CREATING = DatasetContentState' "CREATING"

pattern DatasetContentState_FAILED :: DatasetContentState
pattern DatasetContentState_FAILED = DatasetContentState' "FAILED"

pattern DatasetContentState_SUCCEEDED :: DatasetContentState
pattern DatasetContentState_SUCCEEDED = DatasetContentState' "SUCCEEDED"

{-# COMPLETE
  DatasetContentState_CREATING,
  DatasetContentState_FAILED,
  DatasetContentState_SUCCEEDED,
  DatasetContentState'
  #-}
