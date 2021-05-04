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
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInsertionMode
  ( MotionImageInsertionMode
      ( ..,
        MotionImageInsertionMode_MOV,
        MotionImageInsertionMode_PNG
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Choose the type of motion graphic asset that you are providing for your
-- overlay. You can choose either a .mov file or a series of .png files.
newtype MotionImageInsertionMode = MotionImageInsertionMode'
  { fromMotionImageInsertionMode ::
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

pattern MotionImageInsertionMode_MOV :: MotionImageInsertionMode
pattern MotionImageInsertionMode_MOV = MotionImageInsertionMode' "MOV"

pattern MotionImageInsertionMode_PNG :: MotionImageInsertionMode
pattern MotionImageInsertionMode_PNG = MotionImageInsertionMode' "PNG"

{-# COMPLETE
  MotionImageInsertionMode_MOV,
  MotionImageInsertionMode_PNG,
  MotionImageInsertionMode'
  #-}
