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
-- Module      : Amazonka.MediaLive.Types.Mpeg2DisplayRatio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Mpeg2DisplayRatio
  ( Mpeg2DisplayRatio
      ( ..,
        Mpeg2DisplayRatio_DISPLAYRATIO16X9,
        Mpeg2DisplayRatio_DISPLAYRATIO4X3
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Mpeg2 Display Ratio
newtype Mpeg2DisplayRatio = Mpeg2DisplayRatio'
  { fromMpeg2DisplayRatio ::
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

pattern Mpeg2DisplayRatio_DISPLAYRATIO16X9 :: Mpeg2DisplayRatio
pattern Mpeg2DisplayRatio_DISPLAYRATIO16X9 = Mpeg2DisplayRatio' "DISPLAYRATIO16X9"

pattern Mpeg2DisplayRatio_DISPLAYRATIO4X3 :: Mpeg2DisplayRatio
pattern Mpeg2DisplayRatio_DISPLAYRATIO4X3 = Mpeg2DisplayRatio' "DISPLAYRATIO4X3"

{-# COMPLETE
  Mpeg2DisplayRatio_DISPLAYRATIO16X9,
  Mpeg2DisplayRatio_DISPLAYRATIO4X3,
  Mpeg2DisplayRatio'
  #-}
