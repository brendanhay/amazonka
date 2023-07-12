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
-- Module      : Amazonka.MediaLive.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H265SceneChangeDetect
  ( H265SceneChangeDetect
      ( ..,
        H265SceneChangeDetect_DISABLED,
        H265SceneChangeDetect_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H265 Scene Change Detect
newtype H265SceneChangeDetect = H265SceneChangeDetect'
  { fromH265SceneChangeDetect ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
