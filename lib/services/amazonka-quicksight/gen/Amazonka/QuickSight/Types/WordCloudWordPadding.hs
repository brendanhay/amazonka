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
-- Module      : Amazonka.QuickSight.Types.WordCloudWordPadding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudWordPadding
  ( WordCloudWordPadding
      ( ..,
        WordCloudWordPadding_LARGE,
        WordCloudWordPadding_MEDIUM,
        WordCloudWordPadding_NONE,
        WordCloudWordPadding_SMALL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WordCloudWordPadding = WordCloudWordPadding'
  { fromWordCloudWordPadding ::
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

pattern WordCloudWordPadding_LARGE :: WordCloudWordPadding
pattern WordCloudWordPadding_LARGE = WordCloudWordPadding' "LARGE"

pattern WordCloudWordPadding_MEDIUM :: WordCloudWordPadding
pattern WordCloudWordPadding_MEDIUM = WordCloudWordPadding' "MEDIUM"

pattern WordCloudWordPadding_NONE :: WordCloudWordPadding
pattern WordCloudWordPadding_NONE = WordCloudWordPadding' "NONE"

pattern WordCloudWordPadding_SMALL :: WordCloudWordPadding
pattern WordCloudWordPadding_SMALL = WordCloudWordPadding' "SMALL"

{-# COMPLETE
  WordCloudWordPadding_LARGE,
  WordCloudWordPadding_MEDIUM,
  WordCloudWordPadding_NONE,
  WordCloudWordPadding_SMALL,
  WordCloudWordPadding'
  #-}
