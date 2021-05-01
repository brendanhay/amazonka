{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedScte20Detection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedScte20Detection
  ( EmbeddedScte20Detection
      ( ..,
        EmbeddedScte20Detection_AUTO,
        EmbeddedScte20Detection_OFF
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Embedded Scte20 Detection
newtype EmbeddedScte20Detection = EmbeddedScte20Detection'
  { fromEmbeddedScte20Detection ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
