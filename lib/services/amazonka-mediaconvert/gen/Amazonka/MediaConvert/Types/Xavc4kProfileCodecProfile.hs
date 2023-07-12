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
-- Module      : Amazonka.MediaConvert.Types.Xavc4kProfileCodecProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Xavc4kProfileCodecProfile
  ( Xavc4kProfileCodecProfile
      ( ..,
        Xavc4kProfileCodecProfile_HIGH,
        Xavc4kProfileCodecProfile_HIGH_422
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the codec profile for this output. Choose High, 8-bit, 4:2:0
-- (HIGH) or High, 10-bit, 4:2:2 (HIGH_422). These profiles are specified
-- in ITU-T H.264.
newtype Xavc4kProfileCodecProfile = Xavc4kProfileCodecProfile'
  { fromXavc4kProfileCodecProfile ::
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

pattern Xavc4kProfileCodecProfile_HIGH :: Xavc4kProfileCodecProfile
pattern Xavc4kProfileCodecProfile_HIGH = Xavc4kProfileCodecProfile' "HIGH"

pattern Xavc4kProfileCodecProfile_HIGH_422 :: Xavc4kProfileCodecProfile
pattern Xavc4kProfileCodecProfile_HIGH_422 = Xavc4kProfileCodecProfile' "HIGH_422"

{-# COMPLETE
  Xavc4kProfileCodecProfile_HIGH,
  Xavc4kProfileCodecProfile_HIGH_422,
  Xavc4kProfileCodecProfile'
  #-}
