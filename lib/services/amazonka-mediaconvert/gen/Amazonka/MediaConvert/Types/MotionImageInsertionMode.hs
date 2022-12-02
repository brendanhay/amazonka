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
-- Module      : Amazonka.MediaConvert.Types.MotionImageInsertionMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MotionImageInsertionMode
  ( MotionImageInsertionMode
      ( ..,
        MotionImageInsertionMode_MOV,
        MotionImageInsertionMode_PNG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the type of motion graphic asset that you are providing for your
-- overlay. You can choose either a .mov file or a series of .png files.
newtype MotionImageInsertionMode = MotionImageInsertionMode'
  { fromMotionImageInsertionMode ::
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

pattern MotionImageInsertionMode_MOV :: MotionImageInsertionMode
pattern MotionImageInsertionMode_MOV = MotionImageInsertionMode' "MOV"

pattern MotionImageInsertionMode_PNG :: MotionImageInsertionMode
pattern MotionImageInsertionMode_PNG = MotionImageInsertionMode' "PNG"

{-# COMPLETE
  MotionImageInsertionMode_MOV,
  MotionImageInsertionMode_PNG,
  MotionImageInsertionMode'
  #-}
