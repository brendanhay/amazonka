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
-- Module      : Network.AWS.MediaLive.Types.AfdSignaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AfdSignaling
  ( AfdSignaling
      ( ..,
        AfdSignaling_AUTO,
        AfdSignaling_FIXED,
        AfdSignaling_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Afd Signaling
newtype AfdSignaling = AfdSignaling'
  { fromAfdSignaling ::
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

pattern AfdSignaling_AUTO :: AfdSignaling
pattern AfdSignaling_AUTO = AfdSignaling' "AUTO"

pattern AfdSignaling_FIXED :: AfdSignaling
pattern AfdSignaling_FIXED = AfdSignaling' "FIXED"

pattern AfdSignaling_NONE :: AfdSignaling
pattern AfdSignaling_NONE = AfdSignaling' "NONE"

{-# COMPLETE
  AfdSignaling_AUTO,
  AfdSignaling_FIXED,
  AfdSignaling_NONE,
  AfdSignaling'
  #-}
