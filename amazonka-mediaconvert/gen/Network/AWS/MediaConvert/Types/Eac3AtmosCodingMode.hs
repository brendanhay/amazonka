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
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
  ( Eac3AtmosCodingMode
      ( ..,
        Eac3AtmosCodingMode_CODING_MODE_9_1_6
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6
-- (CODING_MODE_9_1_6).
newtype Eac3AtmosCodingMode = Eac3AtmosCodingMode'
  { fromEac3AtmosCodingMode ::
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

pattern Eac3AtmosCodingMode_CODING_MODE_9_1_6 :: Eac3AtmosCodingMode
pattern Eac3AtmosCodingMode_CODING_MODE_9_1_6 = Eac3AtmosCodingMode' "CODING_MODE_9_1_6"

{-# COMPLETE
  Eac3AtmosCodingMode_CODING_MODE_9_1_6,
  Eac3AtmosCodingMode'
  #-}
