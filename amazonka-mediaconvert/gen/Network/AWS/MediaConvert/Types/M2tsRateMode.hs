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
-- Module      : Network.AWS.MediaConvert.Types.M2tsRateMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsRateMode
  ( M2tsRateMode
      ( ..,
        M2tsRateMode_CBR,
        M2tsRateMode_VBR
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | When set to CBR, inserts null packets into transport stream to fill
-- specified bitrate. When set to VBR, the bitrate setting acts as the
-- maximum bitrate, but the output will not be padded up to that bitrate.
newtype M2tsRateMode = M2tsRateMode'
  { fromM2tsRateMode ::
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

pattern M2tsRateMode_CBR :: M2tsRateMode
pattern M2tsRateMode_CBR = M2tsRateMode' "CBR"

pattern M2tsRateMode_VBR :: M2tsRateMode
pattern M2tsRateMode_VBR = M2tsRateMode' "VBR"

{-# COMPLETE
  M2tsRateMode_CBR,
  M2tsRateMode_VBR,
  M2tsRateMode'
  #-}
