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
-- Module      : Amazonka.MediaLive.Types.H265ColorMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H265ColorMetadata
  ( H265ColorMetadata
      ( ..,
        H265ColorMetadata_IGNORE,
        H265ColorMetadata_INSERT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H265 Color Metadata
newtype H265ColorMetadata = H265ColorMetadata'
  { fromH265ColorMetadata ::
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

pattern H265ColorMetadata_IGNORE :: H265ColorMetadata
pattern H265ColorMetadata_IGNORE = H265ColorMetadata' "IGNORE"

pattern H265ColorMetadata_INSERT :: H265ColorMetadata
pattern H265ColorMetadata_INSERT = H265ColorMetadata' "INSERT"

{-# COMPLETE
  H265ColorMetadata_IGNORE,
  H265ColorMetadata_INSERT,
  H265ColorMetadata'
  #-}
