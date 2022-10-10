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
-- Module      : Amazonka.MediaLive.Types.M2tsPcrControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M2tsPcrControl
  ( M2tsPcrControl
      ( ..,
        M2tsPcrControl_CONFIGURED_PCR_PERIOD,
        M2tsPcrControl_PCR_EVERY_PES_PACKET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | M2ts Pcr Control
newtype M2tsPcrControl = M2tsPcrControl'
  { fromM2tsPcrControl ::
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

pattern M2tsPcrControl_CONFIGURED_PCR_PERIOD :: M2tsPcrControl
pattern M2tsPcrControl_CONFIGURED_PCR_PERIOD = M2tsPcrControl' "CONFIGURED_PCR_PERIOD"

pattern M2tsPcrControl_PCR_EVERY_PES_PACKET :: M2tsPcrControl
pattern M2tsPcrControl_PCR_EVERY_PES_PACKET = M2tsPcrControl' "PCR_EVERY_PES_PACKET"

{-# COMPLETE
  M2tsPcrControl_CONFIGURED_PCR_PERIOD,
  M2tsPcrControl_PCR_EVERY_PES_PACKET,
  M2tsPcrControl'
  #-}
