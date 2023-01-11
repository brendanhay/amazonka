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
-- Module      : Amazonka.MediaConnect.Types.Colorimetry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Colorimetry
  ( Colorimetry
      ( ..,
        Colorimetry_BT2020,
        Colorimetry_BT2100,
        Colorimetry_BT601,
        Colorimetry_BT709,
        Colorimetry_ST2065_1,
        Colorimetry_ST2065_3,
        Colorimetry_XYZ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Colorimetry = Colorimetry'
  { fromColorimetry ::
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

pattern Colorimetry_BT2020 :: Colorimetry
pattern Colorimetry_BT2020 = Colorimetry' "BT2020"

pattern Colorimetry_BT2100 :: Colorimetry
pattern Colorimetry_BT2100 = Colorimetry' "BT2100"

pattern Colorimetry_BT601 :: Colorimetry
pattern Colorimetry_BT601 = Colorimetry' "BT601"

pattern Colorimetry_BT709 :: Colorimetry
pattern Colorimetry_BT709 = Colorimetry' "BT709"

pattern Colorimetry_ST2065_1 :: Colorimetry
pattern Colorimetry_ST2065_1 = Colorimetry' "ST2065-1"

pattern Colorimetry_ST2065_3 :: Colorimetry
pattern Colorimetry_ST2065_3 = Colorimetry' "ST2065-3"

pattern Colorimetry_XYZ :: Colorimetry
pattern Colorimetry_XYZ = Colorimetry' "XYZ"

{-# COMPLETE
  Colorimetry_BT2020,
  Colorimetry_BT2100,
  Colorimetry_BT601,
  Colorimetry_BT709,
  Colorimetry_ST2065_1,
  Colorimetry_ST2065_3,
  Colorimetry_XYZ,
  Colorimetry'
  #-}
