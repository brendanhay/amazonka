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
-- Module      : Amazonka.MediaConvert.Types.HlsOfflineEncrypted
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsOfflineEncrypted
  ( HlsOfflineEncrypted
      ( ..,
        HlsOfflineEncrypted_DISABLED,
        HlsOfflineEncrypted_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enable this setting to insert the EXT-X-SESSION-KEY element into the
-- master playlist. This allows for offline Apple HLS FairPlay content
-- protection.
newtype HlsOfflineEncrypted = HlsOfflineEncrypted'
  { fromHlsOfflineEncrypted ::
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

pattern HlsOfflineEncrypted_DISABLED :: HlsOfflineEncrypted
pattern HlsOfflineEncrypted_DISABLED = HlsOfflineEncrypted' "DISABLED"

pattern HlsOfflineEncrypted_ENABLED :: HlsOfflineEncrypted
pattern HlsOfflineEncrypted_ENABLED = HlsOfflineEncrypted' "ENABLED"

{-# COMPLETE
  HlsOfflineEncrypted_DISABLED,
  HlsOfflineEncrypted_ENABLED,
  HlsOfflineEncrypted'
  #-}
