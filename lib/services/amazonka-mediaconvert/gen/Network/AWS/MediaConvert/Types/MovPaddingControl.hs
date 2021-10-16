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
-- Module      : Network.AWS.MediaConvert.Types.MovPaddingControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovPaddingControl
  ( MovPaddingControl
      ( ..,
        MovPaddingControl_NONE,
        MovPaddingControl_OMNEON
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | To make this output compatible with Omenon, keep the default value,
-- OMNEON. Unless you need Omneon compatibility, set this value to NONE.
-- When you keep the default value, OMNEON, MediaConvert increases the
-- length of the edit list atom. This might cause file rejections when a
-- recipient of the output file doesn\'t expct this extra padding.
newtype MovPaddingControl = MovPaddingControl'
  { fromMovPaddingControl ::
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

pattern MovPaddingControl_NONE :: MovPaddingControl
pattern MovPaddingControl_NONE = MovPaddingControl' "NONE"

pattern MovPaddingControl_OMNEON :: MovPaddingControl
pattern MovPaddingControl_OMNEON = MovPaddingControl' "OMNEON"

{-# COMPLETE
  MovPaddingControl_NONE,
  MovPaddingControl_OMNEON,
  MovPaddingControl'
  #-}
