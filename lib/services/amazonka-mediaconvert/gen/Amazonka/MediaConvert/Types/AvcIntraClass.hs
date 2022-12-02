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
-- Module      : Amazonka.MediaConvert.Types.AvcIntraClass
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AvcIntraClass
  ( AvcIntraClass
      ( ..,
        AvcIntraClass_CLASS_100,
        AvcIntraClass_CLASS_200,
        AvcIntraClass_CLASS_4K_2K,
        AvcIntraClass_CLASS_50
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the AVC-Intra class of your output. The AVC-Intra class
-- selection determines the output video bit rate depending on the frame
-- rate of the output. Outputs with higher class values have higher
-- bitrates and improved image quality. Note that for Class 4K\/2K,
-- MediaConvert supports only 4:2:2 chroma subsampling.
newtype AvcIntraClass = AvcIntraClass'
  { fromAvcIntraClass ::
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

pattern AvcIntraClass_CLASS_100 :: AvcIntraClass
pattern AvcIntraClass_CLASS_100 = AvcIntraClass' "CLASS_100"

pattern AvcIntraClass_CLASS_200 :: AvcIntraClass
pattern AvcIntraClass_CLASS_200 = AvcIntraClass' "CLASS_200"

pattern AvcIntraClass_CLASS_4K_2K :: AvcIntraClass
pattern AvcIntraClass_CLASS_4K_2K = AvcIntraClass' "CLASS_4K_2K"

pattern AvcIntraClass_CLASS_50 :: AvcIntraClass
pattern AvcIntraClass_CLASS_50 = AvcIntraClass' "CLASS_50"

{-# COMPLETE
  AvcIntraClass_CLASS_100,
  AvcIntraClass_CLASS_200,
  AvcIntraClass_CLASS_4K_2K,
  AvcIntraClass_CLASS_50,
  AvcIntraClass'
  #-}
