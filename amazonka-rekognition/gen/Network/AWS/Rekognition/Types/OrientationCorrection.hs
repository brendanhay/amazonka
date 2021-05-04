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

import qualified Network.AWS.Prelude as Prelude

newtype OrientationCorrection = OrientationCorrection'
  { fromOrientationCorrection ::
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
