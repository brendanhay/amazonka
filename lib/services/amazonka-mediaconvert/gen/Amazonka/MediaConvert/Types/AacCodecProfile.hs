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
-- Module      : Amazonka.MediaConvert.Types.AacCodecProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AacCodecProfile
  ( AacCodecProfile
      ( ..,
        AacCodecProfile_HEV1,
        AacCodecProfile_HEV2,
        AacCodecProfile_LC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | AAC Profile.
newtype AacCodecProfile = AacCodecProfile'
  { fromAacCodecProfile ::
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

pattern AacCodecProfile_HEV1 :: AacCodecProfile
pattern AacCodecProfile_HEV1 = AacCodecProfile' "HEV1"

pattern AacCodecProfile_HEV2 :: AacCodecProfile
pattern AacCodecProfile_HEV2 = AacCodecProfile' "HEV2"

pattern AacCodecProfile_LC :: AacCodecProfile
pattern AacCodecProfile_LC = AacCodecProfile' "LC"

{-# COMPLETE
  AacCodecProfile_HEV1,
  AacCodecProfile_HEV2,
  AacCodecProfile_LC,
  AacCodecProfile'
  #-}
