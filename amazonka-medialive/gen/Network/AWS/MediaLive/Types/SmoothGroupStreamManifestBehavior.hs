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
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior
  ( SmoothGroupStreamManifestBehavior
      ( ..,
        SmoothGroupStreamManifestBehavior_DO_NOT_SEND,
        SmoothGroupStreamManifestBehavior_SEND
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Smooth Group Stream Manifest Behavior
newtype SmoothGroupStreamManifestBehavior = SmoothGroupStreamManifestBehavior'
  { fromSmoothGroupStreamManifestBehavior ::
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

pattern SmoothGroupStreamManifestBehavior_DO_NOT_SEND :: SmoothGroupStreamManifestBehavior
pattern SmoothGroupStreamManifestBehavior_DO_NOT_SEND = SmoothGroupStreamManifestBehavior' "DO_NOT_SEND"

pattern SmoothGroupStreamManifestBehavior_SEND :: SmoothGroupStreamManifestBehavior
pattern SmoothGroupStreamManifestBehavior_SEND = SmoothGroupStreamManifestBehavior' "SEND"

{-# COMPLETE
  SmoothGroupStreamManifestBehavior_DO_NOT_SEND,
  SmoothGroupStreamManifestBehavior_SEND,
  SmoothGroupStreamManifestBehavior'
  #-}
