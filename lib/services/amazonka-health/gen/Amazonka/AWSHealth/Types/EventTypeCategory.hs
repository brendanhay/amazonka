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
-- Module      : Amazonka.AWSHealth.Types.EventTypeCategory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EventTypeCategory
  ( EventTypeCategory
      ( ..,
        EventTypeCategory_AccountNotification,
        EventTypeCategory_Investigation,
        EventTypeCategory_Issue,
        EventTypeCategory_ScheduledChange
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventTypeCategory = EventTypeCategory'
  { fromEventTypeCategory ::
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
