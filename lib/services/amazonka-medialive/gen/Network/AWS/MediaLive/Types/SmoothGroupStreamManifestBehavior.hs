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
-- Module      : Amazonka.MediaLive.Types.SmoothGroupStreamManifestBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.SmoothGroupStreamManifestBehavior
  ( SmoothGroupStreamManifestBehavior
      ( ..,
        SmoothGroupStreamManifestBehavior_DO_NOT_SEND,
        SmoothGroupStreamManifestBehavior_SEND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Smooth Group Stream Manifest Behavior
newtype SmoothGroupStreamManifestBehavior = SmoothGroupStreamManifestBehavior'
  { fromSmoothGroupStreamManifestBehavior ::
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

pattern SmoothGroupStreamManifestBehavior_DO_NOT_SEND :: SmoothGroupStreamManifestBehavior
pattern SmoothGroupStreamManifestBehavior_DO_NOT_SEND = SmoothGroupStreamManifestBehavior' "DO_NOT_SEND"

pattern SmoothGroupStreamManifestBehavior_SEND :: SmoothGroupStreamManifestBehavior
pattern SmoothGroupStreamManifestBehavior_SEND = SmoothGroupStreamManifestBehavior' "SEND"

{-# COMPLETE
  SmoothGroupStreamManifestBehavior_DO_NOT_SEND,
  SmoothGroupStreamManifestBehavior_SEND,
  SmoothGroupStreamManifestBehavior'
  #-}
