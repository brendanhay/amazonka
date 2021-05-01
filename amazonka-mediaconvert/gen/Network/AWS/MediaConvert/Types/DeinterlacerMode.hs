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
-- Module      : Network.AWS.MediaConvert.Types.DeinterlacerMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DeinterlacerMode
  ( DeinterlacerMode
      ( ..,
        DeinterlacerMode_ADAPTIVE,
        DeinterlacerMode_DEINTERLACE,
        DeinterlacerMode_INVERSE_TELECINE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do
-- deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced
-- to progressive. - Inverse telecine converts Hard Telecine 29.97i to
-- progressive 23.976p. - Adaptive auto-detects and converts to
-- progressive.
newtype DeinterlacerMode = DeinterlacerMode'
  { fromDeinterlacerMode ::
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

pattern DeinterlacerMode_ADAPTIVE :: DeinterlacerMode
pattern DeinterlacerMode_ADAPTIVE = DeinterlacerMode' "ADAPTIVE"

pattern DeinterlacerMode_DEINTERLACE :: DeinterlacerMode
pattern DeinterlacerMode_DEINTERLACE = DeinterlacerMode' "DEINTERLACE"

pattern DeinterlacerMode_INVERSE_TELECINE :: DeinterlacerMode
pattern DeinterlacerMode_INVERSE_TELECINE = DeinterlacerMode' "INVERSE_TELECINE"

{-# COMPLETE
  DeinterlacerMode_ADAPTIVE,
  DeinterlacerMode_DEINTERLACE,
  DeinterlacerMode_INVERSE_TELECINE,
  DeinterlacerMode'
  #-}
