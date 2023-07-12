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
-- Module      : Amazonka.MediaConvert.Types.Xavc4kProfileBitrateClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Xavc4kProfileBitrateClass
  ( Xavc4kProfileBitrateClass
      ( ..,
        Xavc4kProfileBitrateClass_BITRATE_CLASS_100,
        Xavc4kProfileBitrateClass_BITRATE_CLASS_140,
        Xavc4kProfileBitrateClass_BITRATE_CLASS_200
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the XAVC 4k (Long GOP) Bitrate Class to set the bitrate of your
-- output. Outputs of the same class have similar image quality over the
-- operating points that are valid for that class.
newtype Xavc4kProfileBitrateClass = Xavc4kProfileBitrateClass'
  { fromXavc4kProfileBitrateClass ::
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

pattern Xavc4kProfileBitrateClass_BITRATE_CLASS_100 :: Xavc4kProfileBitrateClass
pattern Xavc4kProfileBitrateClass_BITRATE_CLASS_100 = Xavc4kProfileBitrateClass' "BITRATE_CLASS_100"

pattern Xavc4kProfileBitrateClass_BITRATE_CLASS_140 :: Xavc4kProfileBitrateClass
pattern Xavc4kProfileBitrateClass_BITRATE_CLASS_140 = Xavc4kProfileBitrateClass' "BITRATE_CLASS_140"

pattern Xavc4kProfileBitrateClass_BITRATE_CLASS_200 :: Xavc4kProfileBitrateClass
pattern Xavc4kProfileBitrateClass_BITRATE_CLASS_200 = Xavc4kProfileBitrateClass' "BITRATE_CLASS_200"

{-# COMPLETE
  Xavc4kProfileBitrateClass_BITRATE_CLASS_100,
  Xavc4kProfileBitrateClass_BITRATE_CLASS_140,
  Xavc4kProfileBitrateClass_BITRATE_CLASS_200,
  Xavc4kProfileBitrateClass'
  #-}
