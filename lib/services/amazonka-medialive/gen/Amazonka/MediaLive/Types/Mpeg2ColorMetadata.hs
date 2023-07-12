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
-- Module      : Amazonka.MediaLive.Types.Mpeg2ColorMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Mpeg2ColorMetadata
  ( Mpeg2ColorMetadata
      ( ..,
        Mpeg2ColorMetadata_IGNORE,
        Mpeg2ColorMetadata_INSERT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Mpeg2 Color Metadata
newtype Mpeg2ColorMetadata = Mpeg2ColorMetadata'
  { fromMpeg2ColorMetadata ::
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

pattern Mpeg2ColorMetadata_IGNORE :: Mpeg2ColorMetadata
pattern Mpeg2ColorMetadata_IGNORE = Mpeg2ColorMetadata' "IGNORE"

pattern Mpeg2ColorMetadata_INSERT :: Mpeg2ColorMetadata
pattern Mpeg2ColorMetadata_INSERT = Mpeg2ColorMetadata' "INSERT"

{-# COMPLETE
  Mpeg2ColorMetadata_IGNORE,
  Mpeg2ColorMetadata_INSERT,
  Mpeg2ColorMetadata'
  #-}
