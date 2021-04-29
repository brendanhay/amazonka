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
-- Module      : Network.AWS.MediaLive.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3StereoDownmix
  ( Eac3StereoDownmix
      ( ..,
        Eac3StereoDownmix_DPL2,
        Eac3StereoDownmix_LO_RO,
        Eac3StereoDownmix_LT_RT,
        Eac3StereoDownmix_NOT_INDICATED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Eac3 Stereo Downmix
newtype Eac3StereoDownmix = Eac3StereoDownmix'
  { fromEac3StereoDownmix ::
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

pattern Eac3StereoDownmix_DPL2 :: Eac3StereoDownmix
pattern Eac3StereoDownmix_DPL2 = Eac3StereoDownmix' "DPL2"

pattern Eac3StereoDownmix_LO_RO :: Eac3StereoDownmix
pattern Eac3StereoDownmix_LO_RO = Eac3StereoDownmix' "LO_RO"

pattern Eac3StereoDownmix_LT_RT :: Eac3StereoDownmix
pattern Eac3StereoDownmix_LT_RT = Eac3StereoDownmix' "LT_RT"

pattern Eac3StereoDownmix_NOT_INDICATED :: Eac3StereoDownmix
pattern Eac3StereoDownmix_NOT_INDICATED = Eac3StereoDownmix' "NOT_INDICATED"

{-# COMPLETE
  Eac3StereoDownmix_DPL2,
  Eac3StereoDownmix_LO_RO,
  Eac3StereoDownmix_LT_RT,
  Eac3StereoDownmix_NOT_INDICATED,
  Eac3StereoDownmix'
  #-}
