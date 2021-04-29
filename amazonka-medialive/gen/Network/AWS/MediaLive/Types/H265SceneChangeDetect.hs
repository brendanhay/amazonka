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
-- Module      : Network.AWS.MediaLive.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265SceneChangeDetect
  ( H265SceneChangeDetect
      ( ..,
        H265SceneChangeDetect_DISABLED,
        H265SceneChangeDetect_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | H265 Scene Change Detect
newtype H265SceneChangeDetect = H265SceneChangeDetect'
  { fromH265SceneChangeDetect ::
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

pattern H265SceneChangeDetect_DISABLED :: H265SceneChangeDetect
pattern H265SceneChangeDetect_DISABLED = H265SceneChangeDetect' "DISABLED"

pattern H265SceneChangeDetect_ENABLED :: H265SceneChangeDetect
pattern H265SceneChangeDetect_ENABLED = H265SceneChangeDetect' "ENABLED"

{-# COMPLETE
  H265SceneChangeDetect_DISABLED,
  H265SceneChangeDetect_ENABLED,
  H265SceneChangeDetect'
  #-}
