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
-- Module      : Amazonka.MediaConvert.Types.Av1BitDepth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Av1BitDepth
  ( Av1BitDepth
      ( ..,
        Av1BitDepth_BIT_10,
        Av1BitDepth_BIT_8
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the Bit depth (Av1BitDepth). You can choose 8-bit (BIT_8) or
-- 10-bit (BIT_10).
newtype Av1BitDepth = Av1BitDepth'
  { fromAv1BitDepth ::
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

pattern Av1BitDepth_BIT_10 :: Av1BitDepth
pattern Av1BitDepth_BIT_10 = Av1BitDepth' "BIT_10"

pattern Av1BitDepth_BIT_8 :: Av1BitDepth
pattern Av1BitDepth_BIT_8 = Av1BitDepth' "BIT_8"

{-# COMPLETE
  Av1BitDepth_BIT_10,
  Av1BitDepth_BIT_8,
  Av1BitDepth'
  #-}
