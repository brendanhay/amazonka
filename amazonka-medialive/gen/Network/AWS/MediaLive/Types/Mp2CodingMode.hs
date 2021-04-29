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
-- Module      : Network.AWS.MediaLive.Types.Mp2CodingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mp2CodingMode
  ( Mp2CodingMode
      ( ..,
        Mp2CodingMode_CODING_MODE_1_0,
        Mp2CodingMode_CODING_MODE_2_0
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Mp2 Coding Mode
newtype Mp2CodingMode = Mp2CodingMode'
  { fromMp2CodingMode ::
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

pattern Mp2CodingMode_CODING_MODE_1_0 :: Mp2CodingMode
pattern Mp2CodingMode_CODING_MODE_1_0 = Mp2CodingMode' "CODING_MODE_1_0"

pattern Mp2CodingMode_CODING_MODE_2_0 :: Mp2CodingMode
pattern Mp2CodingMode_CODING_MODE_2_0 = Mp2CodingMode' "CODING_MODE_2_0"

{-# COMPLETE
  Mp2CodingMode_CODING_MODE_1_0,
  Mp2CodingMode_CODING_MODE_2_0,
  Mp2CodingMode'
  #-}
