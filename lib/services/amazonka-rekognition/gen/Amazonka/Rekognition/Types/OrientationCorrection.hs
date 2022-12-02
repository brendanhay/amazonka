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
-- Module      : Amazonka.Rekognition.Types.OrientationCorrection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.OrientationCorrection
  ( OrientationCorrection
      ( ..,
        OrientationCorrection_ROTATE_0,
        OrientationCorrection_ROTATE_180,
        OrientationCorrection_ROTATE_270,
        OrientationCorrection_ROTATE_90
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OrientationCorrection = OrientationCorrection'
  { fromOrientationCorrection ::
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

pattern OrientationCorrection_ROTATE_0 :: OrientationCorrection
pattern OrientationCorrection_ROTATE_0 = OrientationCorrection' "ROTATE_0"

pattern OrientationCorrection_ROTATE_180 :: OrientationCorrection
pattern OrientationCorrection_ROTATE_180 = OrientationCorrection' "ROTATE_180"

pattern OrientationCorrection_ROTATE_270 :: OrientationCorrection
pattern OrientationCorrection_ROTATE_270 = OrientationCorrection' "ROTATE_270"

pattern OrientationCorrection_ROTATE_90 :: OrientationCorrection
pattern OrientationCorrection_ROTATE_90 = OrientationCorrection' "ROTATE_90"

{-# COMPLETE
  OrientationCorrection_ROTATE_0,
  OrientationCorrection_ROTATE_180,
  OrientationCorrection_ROTATE_270,
  OrientationCorrection_ROTATE_90,
  OrientationCorrection'
  #-}
