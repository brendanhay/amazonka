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
-- Module      : Amazonka.S3.Types.IntelligentTieringAccessTier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.IntelligentTieringAccessTier
  ( IntelligentTieringAccessTier
      ( ..,
        IntelligentTieringAccessTier_ARCHIVE_ACCESS,
        IntelligentTieringAccessTier_DEEP_ARCHIVE_ACCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype IntelligentTieringAccessTier = IntelligentTieringAccessTier'
  { fromIntelligentTieringAccessTier ::
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

pattern IntelligentTieringAccessTier_ARCHIVE_ACCESS :: IntelligentTieringAccessTier
pattern IntelligentTieringAccessTier_ARCHIVE_ACCESS = IntelligentTieringAccessTier' "ARCHIVE_ACCESS"

pattern IntelligentTieringAccessTier_DEEP_ARCHIVE_ACCESS :: IntelligentTieringAccessTier
pattern IntelligentTieringAccessTier_DEEP_ARCHIVE_ACCESS = IntelligentTieringAccessTier' "DEEP_ARCHIVE_ACCESS"

{-# COMPLETE
  IntelligentTieringAccessTier_ARCHIVE_ACCESS,
  IntelligentTieringAccessTier_DEEP_ARCHIVE_ACCESS,
  IntelligentTieringAccessTier'
  #-}
