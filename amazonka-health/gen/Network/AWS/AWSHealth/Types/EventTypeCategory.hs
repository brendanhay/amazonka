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
-- Module      : Network.AWS.AWSHealth.Types.EventTypeCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventTypeCategory
  ( EventTypeCategory
      ( ..,
        EventTypeCategory_AccountNotification,
        EventTypeCategory_Investigation,
        EventTypeCategory_Issue,
        EventTypeCategory_ScheduledChange
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EventTypeCategory = EventTypeCategory'
  { fromEventTypeCategory ::
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

pattern EventTypeCategory_AccountNotification :: EventTypeCategory
pattern EventTypeCategory_AccountNotification = EventTypeCategory' "accountNotification"

pattern EventTypeCategory_Investigation :: EventTypeCategory
pattern EventTypeCategory_Investigation = EventTypeCategory' "investigation"

pattern EventTypeCategory_Issue :: EventTypeCategory
pattern EventTypeCategory_Issue = EventTypeCategory' "issue"

pattern EventTypeCategory_ScheduledChange :: EventTypeCategory
pattern EventTypeCategory_ScheduledChange = EventTypeCategory' "scheduledChange"

{-# COMPLETE
  EventTypeCategory_AccountNotification,
  EventTypeCategory_Investigation,
  EventTypeCategory_Issue,
  EventTypeCategory_ScheduledChange,
  EventTypeCategory'
  #-}
