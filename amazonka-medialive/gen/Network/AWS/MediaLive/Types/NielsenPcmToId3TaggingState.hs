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
-- Module      : Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
  ( NielsenPcmToId3TaggingState
      ( ..,
        NielsenPcmToId3TaggingState_DISABLED,
        NielsenPcmToId3TaggingState_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | State of Nielsen PCM to ID3 tagging
newtype NielsenPcmToId3TaggingState = NielsenPcmToId3TaggingState'
  { fromNielsenPcmToId3TaggingState ::
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

pattern NielsenPcmToId3TaggingState_DISABLED :: NielsenPcmToId3TaggingState
pattern NielsenPcmToId3TaggingState_DISABLED = NielsenPcmToId3TaggingState' "DISABLED"

pattern NielsenPcmToId3TaggingState_ENABLED :: NielsenPcmToId3TaggingState
pattern NielsenPcmToId3TaggingState_ENABLED = NielsenPcmToId3TaggingState' "ENABLED"

{-# COMPLETE
  NielsenPcmToId3TaggingState_DISABLED,
  NielsenPcmToId3TaggingState_ENABLED,
  NielsenPcmToId3TaggingState'
  #-}
