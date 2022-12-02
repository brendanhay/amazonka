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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelMessagePersistenceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelMessagePersistenceType
  ( ChannelMessagePersistenceType
      ( ..,
        ChannelMessagePersistenceType_NON_PERSISTENT,
        ChannelMessagePersistenceType_PERSISTENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChannelMessagePersistenceType = ChannelMessagePersistenceType'
  { fromChannelMessagePersistenceType ::
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

pattern ChannelMessagePersistenceType_NON_PERSISTENT :: ChannelMessagePersistenceType
pattern ChannelMessagePersistenceType_NON_PERSISTENT = ChannelMessagePersistenceType' "NON_PERSISTENT"

pattern ChannelMessagePersistenceType_PERSISTENT :: ChannelMessagePersistenceType
pattern ChannelMessagePersistenceType_PERSISTENT = ChannelMessagePersistenceType' "PERSISTENT"

{-# COMPLETE
  ChannelMessagePersistenceType_NON_PERSISTENT,
  ChannelMessagePersistenceType_PERSISTENT,
  ChannelMessagePersistenceType'
  #-}
