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
-- Module      : Amazonka.LexModels.Types.ChannelType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.ChannelType
  ( ChannelType
      ( ..,
        ChannelType_Facebook,
        ChannelType_Kik,
        ChannelType_Slack,
        ChannelType_Twilio_Sms
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChannelType = ChannelType'
  { fromChannelType ::
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

pattern ChannelType_Facebook :: ChannelType
pattern ChannelType_Facebook = ChannelType' "Facebook"

pattern ChannelType_Kik :: ChannelType
pattern ChannelType_Kik = ChannelType' "Kik"

pattern ChannelType_Slack :: ChannelType
pattern ChannelType_Slack = ChannelType' "Slack"

pattern ChannelType_Twilio_Sms :: ChannelType
pattern ChannelType_Twilio_Sms = ChannelType' "Twilio-Sms"

{-# COMPLETE
  ChannelType_Facebook,
  ChannelType_Kik,
  ChannelType_Slack,
  ChannelType_Twilio_Sms,
  ChannelType'
  #-}
