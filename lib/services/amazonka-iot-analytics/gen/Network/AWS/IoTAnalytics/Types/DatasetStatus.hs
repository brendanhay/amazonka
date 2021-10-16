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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetStatus
  ( DatasetStatus
      ( ..,
        DatasetStatus_ACTIVE,
        DatasetStatus_CREATING,
        DatasetStatus_DELETING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DatasetStatus = DatasetStatus'
  { fromDatasetStatus ::
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

pattern DatasetStatus_ACTIVE :: DatasetStatus
pattern DatasetStatus_ACTIVE = DatasetStatus' "ACTIVE"

pattern DatasetStatus_CREATING :: DatasetStatus
pattern DatasetStatus_CREATING = DatasetStatus' "CREATING"

pattern DatasetStatus_DELETING :: DatasetStatus
pattern DatasetStatus_DELETING = DatasetStatus' "DELETING"

{-# COMPLETE
  DatasetStatus_ACTIVE,
  DatasetStatus_CREATING,
  DatasetStatus_DELETING,
  DatasetStatus'
  #-}
