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
-- Module      : Network.AWS.Rekognition.Types.OrientationCorrection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.OrientationCorrection
  ( OrientationCorrection
      ( ..,
        OrientationCorrection_ROTATE_0,
        OrientationCorrection_ROTATE_180,
        OrientationCorrection_ROTATE_270,
        OrientationCorrection_ROTATE_90
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OrientationCorrection = OrientationCorrection'
  { fromOrientationCorrection ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
