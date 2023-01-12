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
-- Module      : Amazonka.QuickSight.Types.WordCloudWordScaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudWordScaling
  ( WordCloudWordScaling
      ( ..,
        WordCloudWordScaling_EMPHASIZE,
        WordCloudWordScaling_NORMAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WordCloudWordScaling = WordCloudWordScaling'
  { fromWordCloudWordScaling ::
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

pattern WordCloudWordScaling_EMPHASIZE :: WordCloudWordScaling
pattern WordCloudWordScaling_EMPHASIZE = WordCloudWordScaling' "EMPHASIZE"

pattern WordCloudWordScaling_NORMAL :: WordCloudWordScaling
pattern WordCloudWordScaling_NORMAL = WordCloudWordScaling' "NORMAL"

{-# COMPLETE
  WordCloudWordScaling_EMPHASIZE,
  WordCloudWordScaling_NORMAL,
  WordCloudWordScaling'
  #-}
