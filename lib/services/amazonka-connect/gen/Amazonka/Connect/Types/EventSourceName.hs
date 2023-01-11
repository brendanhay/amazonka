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
-- Module      : Amazonka.Connect.Types.EventSourceName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EventSourceName
  ( EventSourceName
      ( ..,
        EventSourceName_OnPostCallAnalysisAvailable,
        EventSourceName_OnPostChatAnalysisAvailable,
        EventSourceName_OnRealTimeCallAnalysisAvailable,
        EventSourceName_OnSalesforceCaseCreate,
        EventSourceName_OnZendeskTicketCreate,
        EventSourceName_OnZendeskTicketStatusUpdate
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventSourceName = EventSourceName'
  { fromEventSourceName ::
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

pattern EventSourceName_OnPostCallAnalysisAvailable :: EventSourceName
pattern EventSourceName_OnPostCallAnalysisAvailable = EventSourceName' "OnPostCallAnalysisAvailable"

pattern EventSourceName_OnPostChatAnalysisAvailable :: EventSourceName
pattern EventSourceName_OnPostChatAnalysisAvailable = EventSourceName' "OnPostChatAnalysisAvailable"

pattern EventSourceName_OnRealTimeCallAnalysisAvailable :: EventSourceName
pattern EventSourceName_OnRealTimeCallAnalysisAvailable = EventSourceName' "OnRealTimeCallAnalysisAvailable"

pattern EventSourceName_OnSalesforceCaseCreate :: EventSourceName
pattern EventSourceName_OnSalesforceCaseCreate = EventSourceName' "OnSalesforceCaseCreate"

pattern EventSourceName_OnZendeskTicketCreate :: EventSourceName
pattern EventSourceName_OnZendeskTicketCreate = EventSourceName' "OnZendeskTicketCreate"

pattern EventSourceName_OnZendeskTicketStatusUpdate :: EventSourceName
pattern EventSourceName_OnZendeskTicketStatusUpdate = EventSourceName' "OnZendeskTicketStatusUpdate"

{-# COMPLETE
  EventSourceName_OnPostCallAnalysisAvailable,
  EventSourceName_OnPostChatAnalysisAvailable,
  EventSourceName_OnRealTimeCallAnalysisAvailable,
  EventSourceName_OnSalesforceCaseCreate,
  EventSourceName_OnZendeskTicketCreate,
  EventSourceName_OnZendeskTicketStatusUpdate,
  EventSourceName'
  #-}
