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
-- Module      : Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl
  ( EbuTtDFillLineGapControl
      ( ..,
        EbuTtDFillLineGapControl_DISABLED,
        EbuTtDFillLineGapControl_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Ebu Tt DFill Line Gap Control
newtype EbuTtDFillLineGapControl = EbuTtDFillLineGapControl'
  { fromEbuTtDFillLineGapControl ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern EbuTtDFillLineGapControl_DISABLED :: EbuTtDFillLineGapControl
pattern EbuTtDFillLineGapControl_DISABLED = EbuTtDFillLineGapControl' "DISABLED"

pattern EbuTtDFillLineGapControl_ENABLED :: EbuTtDFillLineGapControl
pattern EbuTtDFillLineGapControl_ENABLED = EbuTtDFillLineGapControl' "ENABLED"

{-# COMPLETE
  EbuTtDFillLineGapControl_DISABLED,
  EbuTtDFillLineGapControl_ENABLED,
  EbuTtDFillLineGapControl'
  #-}
