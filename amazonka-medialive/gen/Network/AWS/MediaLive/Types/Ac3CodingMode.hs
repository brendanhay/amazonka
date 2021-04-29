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
-- Module      : Network.AWS.MediaLive.Types.Ac3CodingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3CodingMode
  ( Ac3CodingMode
      ( ..,
        Ac3CodingMode_CODING_MODE_1_0,
        Ac3CodingMode_CODING_MODE_1_1,
        Ac3CodingMode_CODING_MODE_2_0,
        Ac3CodingMode_CODING_MODE_3_2_LFE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Ac3 Coding Mode
newtype Ac3CodingMode = Ac3CodingMode'
  { fromAc3CodingMode ::
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

pattern Ac3CodingMode_CODING_MODE_1_0 :: Ac3CodingMode
pattern Ac3CodingMode_CODING_MODE_1_0 = Ac3CodingMode' "CODING_MODE_1_0"

pattern Ac3CodingMode_CODING_MODE_1_1 :: Ac3CodingMode
pattern Ac3CodingMode_CODING_MODE_1_1 = Ac3CodingMode' "CODING_MODE_1_1"

pattern Ac3CodingMode_CODING_MODE_2_0 :: Ac3CodingMode
pattern Ac3CodingMode_CODING_MODE_2_0 = Ac3CodingMode' "CODING_MODE_2_0"

pattern Ac3CodingMode_CODING_MODE_3_2_LFE :: Ac3CodingMode
pattern Ac3CodingMode_CODING_MODE_3_2_LFE = Ac3CodingMode' "CODING_MODE_3_2_LFE"

{-# COMPLETE
  Ac3CodingMode_CODING_MODE_1_0,
  Ac3CodingMode_CODING_MODE_1_1,
  Ac3CodingMode_CODING_MODE_2_0,
  Ac3CodingMode_CODING_MODE_3_2_LFE,
  Ac3CodingMode'
  #-}
