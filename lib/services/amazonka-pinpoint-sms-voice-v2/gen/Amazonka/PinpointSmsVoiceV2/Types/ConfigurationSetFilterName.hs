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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetFilterName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetFilterName
  ( ConfigurationSetFilterName
      ( ..,
        ConfigurationSetFilterName_Default_message_type,
        ConfigurationSetFilterName_Default_sender_id,
        ConfigurationSetFilterName_Event_destination_name,
        ConfigurationSetFilterName_Matching_event_types
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigurationSetFilterName = ConfigurationSetFilterName'
  { fromConfigurationSetFilterName ::
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

pattern ConfigurationSetFilterName_Default_message_type :: ConfigurationSetFilterName
pattern ConfigurationSetFilterName_Default_message_type = ConfigurationSetFilterName' "default-message-type"

pattern ConfigurationSetFilterName_Default_sender_id :: ConfigurationSetFilterName
pattern ConfigurationSetFilterName_Default_sender_id = ConfigurationSetFilterName' "default-sender-id"

pattern ConfigurationSetFilterName_Event_destination_name :: ConfigurationSetFilterName
pattern ConfigurationSetFilterName_Event_destination_name = ConfigurationSetFilterName' "event-destination-name"

pattern ConfigurationSetFilterName_Matching_event_types :: ConfigurationSetFilterName
pattern ConfigurationSetFilterName_Matching_event_types = ConfigurationSetFilterName' "matching-event-types"

{-# COMPLETE
  ConfigurationSetFilterName_Default_message_type,
  ConfigurationSetFilterName_Default_sender_id,
  ConfigurationSetFilterName_Event_destination_name,
  ConfigurationSetFilterName_Matching_event_types,
  ConfigurationSetFilterName'
  #-}
