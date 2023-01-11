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
-- Module      : Amazonka.QuickSight.Types.WordCloudCloudLayout
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudCloudLayout
  ( WordCloudCloudLayout
      ( ..,
        WordCloudCloudLayout_FLUID,
        WordCloudCloudLayout_NORMAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WordCloudCloudLayout = WordCloudCloudLayout'
  { fromWordCloudCloudLayout ::
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

pattern WordCloudCloudLayout_FLUID :: WordCloudCloudLayout
pattern WordCloudCloudLayout_FLUID = WordCloudCloudLayout' "FLUID"

pattern WordCloudCloudLayout_NORMAL :: WordCloudCloudLayout
pattern WordCloudCloudLayout_NORMAL = WordCloudCloudLayout' "NORMAL"

{-# COMPLETE
  WordCloudCloudLayout_FLUID,
  WordCloudCloudLayout_NORMAL,
  WordCloudCloudLayout'
  #-}
