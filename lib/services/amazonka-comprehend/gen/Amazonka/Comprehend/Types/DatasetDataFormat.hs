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
-- Module      : Amazonka.Comprehend.Types.DatasetDataFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetDataFormat
  ( DatasetDataFormat
      ( ..,
        DatasetDataFormat_AUGMENTED_MANIFEST,
        DatasetDataFormat_COMPREHEND_CSV
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatasetDataFormat = DatasetDataFormat'
  { fromDatasetDataFormat ::
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

pattern DatasetDataFormat_AUGMENTED_MANIFEST :: DatasetDataFormat
pattern DatasetDataFormat_AUGMENTED_MANIFEST = DatasetDataFormat' "AUGMENTED_MANIFEST"

pattern DatasetDataFormat_COMPREHEND_CSV :: DatasetDataFormat
pattern DatasetDataFormat_COMPREHEND_CSV = DatasetDataFormat' "COMPREHEND_CSV"

{-# COMPLETE
  DatasetDataFormat_AUGMENTED_MANIFEST,
  DatasetDataFormat_COMPREHEND_CSV,
  DatasetDataFormat'
  #-}
