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
-- Module      : Network.AWS.MediaLive.Types.Scte20Convert608To708
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20Convert608To708
  ( Scte20Convert608To708
      ( ..,
        Scte20Convert608To708_DISABLED,
        Scte20Convert608To708_UPCONVERT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Scte20 Convert608 To708
newtype Scte20Convert608To708 = Scte20Convert608To708'
  { fromScte20Convert608To708 ::
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

pattern Scte20Convert608To708_DISABLED :: Scte20Convert608To708
pattern Scte20Convert608To708_DISABLED = Scte20Convert608To708' "DISABLED"

pattern Scte20Convert608To708_UPCONVERT :: Scte20Convert608To708
pattern Scte20Convert608To708_UPCONVERT = Scte20Convert608To708' "UPCONVERT"

{-# COMPLETE
  Scte20Convert608To708_DISABLED,
  Scte20Convert608To708_UPCONVERT,
  Scte20Convert608To708'
  #-}
