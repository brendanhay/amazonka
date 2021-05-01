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
-- Module      : Network.AWS.MediaConvert.Types.Vc3InterlaceMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3InterlaceMode
  ( Vc3InterlaceMode
      ( ..,
        Vc3InterlaceMode_INTERLACED,
        Vc3InterlaceMode_PROGRESSIVE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Optional. Choose the scan line type for this output. If you don\'t
-- specify a value, MediaConvert will create a progressive output.
newtype Vc3InterlaceMode = Vc3InterlaceMode'
  { fromVc3InterlaceMode ::
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

pattern Vc3InterlaceMode_INTERLACED :: Vc3InterlaceMode
pattern Vc3InterlaceMode_INTERLACED = Vc3InterlaceMode' "INTERLACED"

pattern Vc3InterlaceMode_PROGRESSIVE :: Vc3InterlaceMode
pattern Vc3InterlaceMode_PROGRESSIVE = Vc3InterlaceMode' "PROGRESSIVE"

{-# COMPLETE
  Vc3InterlaceMode_INTERLACED,
  Vc3InterlaceMode_PROGRESSIVE,
  Vc3InterlaceMode'
  #-}
