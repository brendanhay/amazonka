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
-- Module      : Amazonka.AlexaBusiness.Types.CommsProtocol
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.CommsProtocol
  ( CommsProtocol
      ( ..,
        CommsProtocol_H323,
        CommsProtocol_SIP,
        CommsProtocol_SIPS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CommsProtocol = CommsProtocol'
  { fromCommsProtocol ::
      Core.Text
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

pattern CommsProtocol_H323 :: CommsProtocol
pattern CommsProtocol_H323 = CommsProtocol' "H323"

pattern CommsProtocol_SIP :: CommsProtocol
pattern CommsProtocol_SIP = CommsProtocol' "SIP"

pattern CommsProtocol_SIPS :: CommsProtocol
pattern CommsProtocol_SIPS = CommsProtocol' "SIPS"

{-# COMPLETE
  CommsProtocol_H323,
  CommsProtocol_SIP,
  CommsProtocol_SIPS,
  CommsProtocol'
  #-}
