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
-- Module      : Network.AWS.MediaConvert.Types.Eac3SurroundMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3SurroundMode
  ( Eac3SurroundMode
      ( ..,
        Eac3SurroundMode_DISABLED,
        Eac3SurroundMode_ENABLED,
        Eac3SurroundMode_NOT_INDICATED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
newtype Eac3SurroundMode = Eac3SurroundMode'
  { fromEac3SurroundMode ::
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

pattern Eac3SurroundMode_DISABLED :: Eac3SurroundMode
pattern Eac3SurroundMode_DISABLED = Eac3SurroundMode' "DISABLED"

pattern Eac3SurroundMode_ENABLED :: Eac3SurroundMode
pattern Eac3SurroundMode_ENABLED = Eac3SurroundMode' "ENABLED"

pattern Eac3SurroundMode_NOT_INDICATED :: Eac3SurroundMode
pattern Eac3SurroundMode_NOT_INDICATED = Eac3SurroundMode' "NOT_INDICATED"

{-# COMPLETE
  Eac3SurroundMode_DISABLED,
  Eac3SurroundMode_ENABLED,
  Eac3SurroundMode_NOT_INDICATED,
  Eac3SurroundMode'
  #-}
