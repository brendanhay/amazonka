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
-- Module      : Network.AWS.CloudWatch.Types.HistoryItemType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.HistoryItemType
  ( HistoryItemType
      ( ..,
        HistoryItemType_Action,
        HistoryItemType_ConfigurationUpdate,
        HistoryItemType_StateUpdate
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HistoryItemType = HistoryItemType'
  { fromHistoryItemType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern HistoryItemType_Action :: HistoryItemType
pattern HistoryItemType_Action = HistoryItemType' "Action"

pattern HistoryItemType_ConfigurationUpdate :: HistoryItemType
pattern HistoryItemType_ConfigurationUpdate = HistoryItemType' "ConfigurationUpdate"

pattern HistoryItemType_StateUpdate :: HistoryItemType
pattern HistoryItemType_StateUpdate = HistoryItemType' "StateUpdate"

{-# COMPLETE
  HistoryItemType_Action,
  HistoryItemType_ConfigurationUpdate,
  HistoryItemType_StateUpdate,
  HistoryItemType'
  #-}
