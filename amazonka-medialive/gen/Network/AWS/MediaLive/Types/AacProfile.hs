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
-- Module      : Network.AWS.MediaLive.Types.AacProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacProfile
  ( AacProfile
      ( ..,
        AacProfile_HEV1,
        AacProfile_HEV2,
        AacProfile_LC
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Aac Profile
newtype AacProfile = AacProfile'
  { fromAacProfile ::
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

pattern AacProfile_HEV1 :: AacProfile
pattern AacProfile_HEV1 = AacProfile' "HEV1"

pattern AacProfile_HEV2 :: AacProfile
pattern AacProfile_HEV2 = AacProfile' "HEV2"

pattern AacProfile_LC :: AacProfile
pattern AacProfile_LC = AacProfile' "LC"

{-# COMPLETE
  AacProfile_HEV1,
  AacProfile_HEV2,
  AacProfile_LC,
  AacProfile'
  #-}
