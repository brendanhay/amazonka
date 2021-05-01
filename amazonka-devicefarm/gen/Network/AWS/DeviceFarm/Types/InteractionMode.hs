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
-- Module      : Network.AWS.DeviceFarm.Types.InteractionMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InteractionMode
  ( InteractionMode
      ( ..,
        InteractionMode_INTERACTIVE,
        InteractionMode_NO_VIDEO,
        InteractionMode_VIDEO_ONLY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype InteractionMode = InteractionMode'
  { fromInteractionMode ::
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

pattern InteractionMode_INTERACTIVE :: InteractionMode
pattern InteractionMode_INTERACTIVE = InteractionMode' "INTERACTIVE"

pattern InteractionMode_NO_VIDEO :: InteractionMode
pattern InteractionMode_NO_VIDEO = InteractionMode' "NO_VIDEO"

pattern InteractionMode_VIDEO_ONLY :: InteractionMode
pattern InteractionMode_VIDEO_ONLY = InteractionMode' "VIDEO_ONLY"

{-# COMPLETE
  InteractionMode_INTERACTIVE,
  InteractionMode_NO_VIDEO,
  InteractionMode_VIDEO_ONLY,
  InteractionMode'
  #-}
