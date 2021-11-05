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
-- Module      : Amazonka.MediaLive.Types.Fmp4TimedMetadataBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Fmp4TimedMetadataBehavior
  ( Fmp4TimedMetadataBehavior
      ( ..,
        Fmp4TimedMetadataBehavior_NO_PASSTHROUGH,
        Fmp4TimedMetadataBehavior_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Fmp4 Timed Metadata Behavior
newtype Fmp4TimedMetadataBehavior = Fmp4TimedMetadataBehavior'
  { fromFmp4TimedMetadataBehavior ::
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

pattern Fmp4TimedMetadataBehavior_NO_PASSTHROUGH :: Fmp4TimedMetadataBehavior
pattern Fmp4TimedMetadataBehavior_NO_PASSTHROUGH = Fmp4TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern Fmp4TimedMetadataBehavior_PASSTHROUGH :: Fmp4TimedMetadataBehavior
pattern Fmp4TimedMetadataBehavior_PASSTHROUGH = Fmp4TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  Fmp4TimedMetadataBehavior_NO_PASSTHROUGH,
  Fmp4TimedMetadataBehavior_PASSTHROUGH,
  Fmp4TimedMetadataBehavior'
  #-}
