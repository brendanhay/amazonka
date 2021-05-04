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
-- Module      : Network.AWS.MediaConvert.Types.ProresCodecProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresCodecProfile
  ( ProresCodecProfile
      ( ..,
        ProresCodecProfile_APPLE_PRORES_422,
        ProresCodecProfile_APPLE_PRORES_422_HQ,
        ProresCodecProfile_APPLE_PRORES_422_LT,
        ProresCodecProfile_APPLE_PRORES_422_PROXY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Use Profile (ProResCodecProfile) to specify the type of Apple ProRes
-- codec to use for this output.
newtype ProresCodecProfile = ProresCodecProfile'
  { fromProresCodecProfile ::
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

pattern ProresCodecProfile_APPLE_PRORES_422 :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422 = ProresCodecProfile' "APPLE_PRORES_422"

pattern ProresCodecProfile_APPLE_PRORES_422_HQ :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_HQ = ProresCodecProfile' "APPLE_PRORES_422_HQ"

pattern ProresCodecProfile_APPLE_PRORES_422_LT :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_LT = ProresCodecProfile' "APPLE_PRORES_422_LT"

pattern ProresCodecProfile_APPLE_PRORES_422_PROXY :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_PROXY = ProresCodecProfile' "APPLE_PRORES_422_PROXY"

{-# COMPLETE
  ProresCodecProfile_APPLE_PRORES_422,
  ProresCodecProfile_APPLE_PRORES_422_HQ,
  ProresCodecProfile_APPLE_PRORES_422_LT,
  ProresCodecProfile_APPLE_PRORES_422_PROXY,
  ProresCodecProfile'
  #-}
