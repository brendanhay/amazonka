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
-- Module      : Network.AWS.MediaLive.Types.Eac3CodingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3CodingMode
  ( Eac3CodingMode
      ( ..,
        Eac3CodingMode_CODING_MODE_1_0,
        Eac3CodingMode_CODING_MODE_2_0,
        Eac3CodingMode_CODING_MODE_3_2
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Eac3 Coding Mode
newtype Eac3CodingMode = Eac3CodingMode'
  { fromEac3CodingMode ::
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

pattern Eac3CodingMode_CODING_MODE_1_0 :: Eac3CodingMode
pattern Eac3CodingMode_CODING_MODE_1_0 = Eac3CodingMode' "CODING_MODE_1_0"

pattern Eac3CodingMode_CODING_MODE_2_0 :: Eac3CodingMode
pattern Eac3CodingMode_CODING_MODE_2_0 = Eac3CodingMode' "CODING_MODE_2_0"

pattern Eac3CodingMode_CODING_MODE_3_2 :: Eac3CodingMode
pattern Eac3CodingMode_CODING_MODE_3_2 = Eac3CodingMode' "CODING_MODE_3_2"

{-# COMPLETE
  Eac3CodingMode_CODING_MODE_1_0,
  Eac3CodingMode_CODING_MODE_2_0,
  Eac3CodingMode_CODING_MODE_3_2,
  Eac3CodingMode'
  #-}
