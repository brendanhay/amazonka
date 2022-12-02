{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.Vc3InterlaceMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Vc3InterlaceMode
  ( Vc3InterlaceMode
      ( ..,
        Vc3InterlaceMode_INTERLACED,
        Vc3InterlaceMode_PROGRESSIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. Choose the scan line type for this output. If you don\'t
-- specify a value, MediaConvert will create a progressive output.
newtype Vc3InterlaceMode = Vc3InterlaceMode'
  { fromVc3InterlaceMode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
