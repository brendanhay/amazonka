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
-- Module      : Network.AWS.MediaLive.Types.EmbeddedConvert608To708
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedConvert608To708
  ( EmbeddedConvert608To708
      ( ..,
        EmbeddedConvert608To708_DISABLED,
        EmbeddedConvert608To708_UPCONVERT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Embedded Convert608 To708
newtype EmbeddedConvert608To708 = EmbeddedConvert608To708'
  { fromEmbeddedConvert608To708 ::
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

pattern EmbeddedConvert608To708_DISABLED :: EmbeddedConvert608To708
pattern EmbeddedConvert608To708_DISABLED = EmbeddedConvert608To708' "DISABLED"

pattern EmbeddedConvert608To708_UPCONVERT :: EmbeddedConvert608To708
pattern EmbeddedConvert608To708_UPCONVERT = EmbeddedConvert608To708' "UPCONVERT"

{-# COMPLETE
  EmbeddedConvert608To708_DISABLED,
  EmbeddedConvert608To708_UPCONVERT,
  EmbeddedConvert608To708'
  #-}
