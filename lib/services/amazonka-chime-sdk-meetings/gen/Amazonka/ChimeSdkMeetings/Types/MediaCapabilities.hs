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
-- Module      : Amazonka.ChimeSdkMeetings.Types.MediaCapabilities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.MediaCapabilities
  ( MediaCapabilities
      ( ..,
        MediaCapabilities_None,
        MediaCapabilities_Receive,
        MediaCapabilities_Send,
        MediaCapabilities_SendReceive
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MediaCapabilities = MediaCapabilities'
  { fromMediaCapabilities ::
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

pattern MediaCapabilities_None :: MediaCapabilities
pattern MediaCapabilities_None = MediaCapabilities' "None"

pattern MediaCapabilities_Receive :: MediaCapabilities
pattern MediaCapabilities_Receive = MediaCapabilities' "Receive"

pattern MediaCapabilities_Send :: MediaCapabilities
pattern MediaCapabilities_Send = MediaCapabilities' "Send"

pattern MediaCapabilities_SendReceive :: MediaCapabilities
pattern MediaCapabilities_SendReceive = MediaCapabilities' "SendReceive"

{-# COMPLETE
  MediaCapabilities_None,
  MediaCapabilities_Receive,
  MediaCapabilities_Send,
  MediaCapabilities_SendReceive,
  MediaCapabilities'
  #-}
