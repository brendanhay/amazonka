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
-- Module      : Amazonka.MediaConvert.Types.ColorMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ColorMetadata
  ( ColorMetadata
      ( ..,
        ColorMetadata_IGNORE,
        ColorMetadata_INSERT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose Insert (INSERT) for this setting to include color metadata in
-- this output. Choose Ignore (IGNORE) to exclude color metadata from this
-- output. If you don\'t specify a value, the service sets this to Insert
-- by default.
newtype ColorMetadata = ColorMetadata'
  { fromColorMetadata ::
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

pattern ColorMetadata_IGNORE :: ColorMetadata
pattern ColorMetadata_IGNORE = ColorMetadata' "IGNORE"

pattern ColorMetadata_INSERT :: ColorMetadata
pattern ColorMetadata_INSERT = ColorMetadata' "INSERT"

{-# COMPLETE
  ColorMetadata_IGNORE,
  ColorMetadata_INSERT,
  ColorMetadata'
  #-}
