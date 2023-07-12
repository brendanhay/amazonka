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
-- Module      : Amazonka.Pinpoint.Types.ChannelType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ChannelType
  ( ChannelType
      ( ..,
        ChannelType_ADM,
        ChannelType_APNS,
        ChannelType_APNS_SANDBOX,
        ChannelType_APNS_VOIP,
        ChannelType_APNS_VOIP_SANDBOX,
        ChannelType_BAIDU,
        ChannelType_CUSTOM,
        ChannelType_EMAIL,
        ChannelType_GCM,
        ChannelType_IN_APP,
        ChannelType_PUSH,
        ChannelType_SMS,
        ChannelType_VOICE
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

pattern ChannelType_ADM :: ChannelType
pattern ChannelType_ADM = ChannelType' "ADM"

pattern ChannelType_APNS :: ChannelType
pattern ChannelType_APNS = ChannelType' "APNS"

pattern ChannelType_APNS_SANDBOX :: ChannelType
pattern ChannelType_APNS_SANDBOX = ChannelType' "APNS_SANDBOX"

pattern ChannelType_APNS_VOIP :: ChannelType
pattern ChannelType_APNS_VOIP = ChannelType' "APNS_VOIP"

pattern ChannelType_APNS_VOIP_SANDBOX :: ChannelType
pattern ChannelType_APNS_VOIP_SANDBOX = ChannelType' "APNS_VOIP_SANDBOX"

pattern ChannelType_BAIDU :: ChannelType
pattern ChannelType_BAIDU = ChannelType' "BAIDU"

pattern ChannelType_CUSTOM :: ChannelType
pattern ChannelType_CUSTOM = ChannelType' "CUSTOM"

pattern ChannelType_EMAIL :: ChannelType
pattern ChannelType_EMAIL = ChannelType' "EMAIL"

pattern ChannelType_GCM :: ChannelType
pattern ChannelType_GCM = ChannelType' "GCM"

pattern ChannelType_IN_APP :: ChannelType
pattern ChannelType_IN_APP = ChannelType' "IN_APP"

pattern ChannelType_PUSH :: ChannelType
pattern ChannelType_PUSH = ChannelType' "PUSH"

pattern ChannelType_SMS :: ChannelType
pattern ChannelType_SMS = ChannelType' "SMS"

pattern ChannelType_VOICE :: ChannelType
pattern ChannelType_VOICE = ChannelType' "VOICE"

{-# COMPLETE
  ChannelType_ADM,
  ChannelType_APNS,
  ChannelType_APNS_SANDBOX,
  ChannelType_APNS_VOIP,
  ChannelType_APNS_VOIP_SANDBOX,
  ChannelType_BAIDU,
  ChannelType_CUSTOM,
  ChannelType_EMAIL,
  ChannelType_GCM,
  ChannelType_IN_APP,
  ChannelType_PUSH,
  ChannelType_SMS,
  ChannelType_VOICE,
  ChannelType'
  #-}
