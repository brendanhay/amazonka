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
-- Module      : Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
  ( UpdateDataRetentionOperation
      ( ..,
        UpdateDataRetentionOperation_DECREASE_DATA_RETENTION,
        UpdateDataRetentionOperation_INCREASE_DATA_RETENTION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype UpdateDataRetentionOperation = UpdateDataRetentionOperation'
  { fromUpdateDataRetentionOperation ::
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

pattern UpdateDataRetentionOperation_DECREASE_DATA_RETENTION :: UpdateDataRetentionOperation
pattern UpdateDataRetentionOperation_DECREASE_DATA_RETENTION = UpdateDataRetentionOperation' "DECREASE_DATA_RETENTION"

pattern UpdateDataRetentionOperation_INCREASE_DATA_RETENTION :: UpdateDataRetentionOperation
pattern UpdateDataRetentionOperation_INCREASE_DATA_RETENTION = UpdateDataRetentionOperation' "INCREASE_DATA_RETENTION"

{-# COMPLETE
  UpdateDataRetentionOperation_DECREASE_DATA_RETENTION,
  UpdateDataRetentionOperation_INCREASE_DATA_RETENTION,
  UpdateDataRetentionOperation'
  #-}
