{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.HistoryItemType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.HistoryItemType
  ( HistoryItemType
      ( HistoryItemType',
        ConfigurationUpdate,
        StateUpdate,
        Action
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HistoryItemType = HistoryItemType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ConfigurationUpdate :: HistoryItemType
pattern ConfigurationUpdate = HistoryItemType' "ConfigurationUpdate"

pattern StateUpdate :: HistoryItemType
pattern StateUpdate = HistoryItemType' "StateUpdate"

pattern Action :: HistoryItemType
pattern Action = HistoryItemType' "Action"

{-# COMPLETE
  ConfigurationUpdate,
  StateUpdate,
  Action,
  HistoryItemType'
  #-}
