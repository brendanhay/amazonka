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
-- Module      : Amazonka.MediaPackageV2.Types.DrmSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.DrmSystem
  ( DrmSystem
      ( ..,
        DrmSystem_CLEAR_KEY_AES_128,
        DrmSystem_FAIRPLAY,
        DrmSystem_PLAYREADY,
        DrmSystem_WIDEVINE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DrmSystem = DrmSystem'
  { fromDrmSystem ::
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

pattern DrmSystem_CLEAR_KEY_AES_128 :: DrmSystem
pattern DrmSystem_CLEAR_KEY_AES_128 = DrmSystem' "CLEAR_KEY_AES_128"

pattern DrmSystem_FAIRPLAY :: DrmSystem
pattern DrmSystem_FAIRPLAY = DrmSystem' "FAIRPLAY"

pattern DrmSystem_PLAYREADY :: DrmSystem
pattern DrmSystem_PLAYREADY = DrmSystem' "PLAYREADY"

pattern DrmSystem_WIDEVINE :: DrmSystem
pattern DrmSystem_WIDEVINE = DrmSystem' "WIDEVINE"

{-# COMPLETE
  DrmSystem_CLEAR_KEY_AES_128,
  DrmSystem_FAIRPLAY,
  DrmSystem_PLAYREADY,
  DrmSystem_WIDEVINE,
  DrmSystem'
  #-}
