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
-- Module      : Network.AWS.CostExplorer.Types.AnomalyFeedbackType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyFeedbackType
  ( AnomalyFeedbackType
      ( ..,
        AnomalyFeedbackType_NO,
        AnomalyFeedbackType_PLANNED_ACTIVITY,
        AnomalyFeedbackType_YES
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AnomalyFeedbackType = AnomalyFeedbackType'
  { fromAnomalyFeedbackType ::
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

pattern AnomalyFeedbackType_NO :: AnomalyFeedbackType
pattern AnomalyFeedbackType_NO = AnomalyFeedbackType' "NO"

pattern AnomalyFeedbackType_PLANNED_ACTIVITY :: AnomalyFeedbackType
pattern AnomalyFeedbackType_PLANNED_ACTIVITY = AnomalyFeedbackType' "PLANNED_ACTIVITY"

pattern AnomalyFeedbackType_YES :: AnomalyFeedbackType
pattern AnomalyFeedbackType_YES = AnomalyFeedbackType' "YES"

{-# COMPLETE
  AnomalyFeedbackType_NO,
  AnomalyFeedbackType_PLANNED_ACTIVITY,
  AnomalyFeedbackType_YES,
  AnomalyFeedbackType'
  #-}
