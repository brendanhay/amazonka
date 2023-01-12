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
-- Module      : Amazonka.MediaLive.Types.EmbeddedScte20Detection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.EmbeddedScte20Detection
  ( EmbeddedScte20Detection
      ( ..,
        EmbeddedScte20Detection_AUTO,
        EmbeddedScte20Detection_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Embedded Scte20 Detection
newtype EmbeddedScte20Detection = EmbeddedScte20Detection'
  { fromEmbeddedScte20Detection ::
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

pattern EmbeddedScte20Detection_AUTO :: EmbeddedScte20Detection
pattern EmbeddedScte20Detection_AUTO = EmbeddedScte20Detection' "AUTO"

pattern EmbeddedScte20Detection_OFF :: EmbeddedScte20Detection
pattern EmbeddedScte20Detection_OFF = EmbeddedScte20Detection' "OFF"

{-# COMPLETE
  EmbeddedScte20Detection_AUTO,
  EmbeddedScte20Detection_OFF,
  EmbeddedScte20Detection'
  #-}
