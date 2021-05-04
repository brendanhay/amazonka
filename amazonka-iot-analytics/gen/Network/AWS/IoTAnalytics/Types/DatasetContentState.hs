{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype DatasetContentState = DatasetContentState'
  { fromDatasetContentState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
