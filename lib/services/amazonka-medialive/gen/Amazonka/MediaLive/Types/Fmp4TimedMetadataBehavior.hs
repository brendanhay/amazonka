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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Fmp4 Timed Metadata Behavior
newtype Fmp4TimedMetadataBehavior = Fmp4TimedMetadataBehavior'
  { fromFmp4TimedMetadataBehavior ::
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

pattern Fmp4TimedMetadataBehavior_NO_PASSTHROUGH :: Fmp4TimedMetadataBehavior
pattern Fmp4TimedMetadataBehavior_NO_PASSTHROUGH = Fmp4TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern Fmp4TimedMetadataBehavior_PASSTHROUGH :: Fmp4TimedMetadataBehavior
pattern Fmp4TimedMetadataBehavior_PASSTHROUGH = Fmp4TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  Fmp4TimedMetadataBehavior_NO_PASSTHROUGH,
  Fmp4TimedMetadataBehavior_PASSTHROUGH,
  Fmp4TimedMetadataBehavior'
  #-}
