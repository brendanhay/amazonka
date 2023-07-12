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
-- Module      : Amazonka.CodeCommit.Types.ReplacementTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ReplacementTypeEnum
  ( ReplacementTypeEnum
      ( ..,
        ReplacementTypeEnum_KEEP_BASE,
        ReplacementTypeEnum_KEEP_DESTINATION,
        ReplacementTypeEnum_KEEP_SOURCE,
        ReplacementTypeEnum_USE_NEW_CONTENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReplacementTypeEnum = ReplacementTypeEnum'
  { fromReplacementTypeEnum ::
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

pattern ReplacementTypeEnum_KEEP_BASE :: ReplacementTypeEnum
pattern ReplacementTypeEnum_KEEP_BASE = ReplacementTypeEnum' "KEEP_BASE"

pattern ReplacementTypeEnum_KEEP_DESTINATION :: ReplacementTypeEnum
pattern ReplacementTypeEnum_KEEP_DESTINATION = ReplacementTypeEnum' "KEEP_DESTINATION"

pattern ReplacementTypeEnum_KEEP_SOURCE :: ReplacementTypeEnum
pattern ReplacementTypeEnum_KEEP_SOURCE = ReplacementTypeEnum' "KEEP_SOURCE"

pattern ReplacementTypeEnum_USE_NEW_CONTENT :: ReplacementTypeEnum
pattern ReplacementTypeEnum_USE_NEW_CONTENT = ReplacementTypeEnum' "USE_NEW_CONTENT"

{-# COMPLETE
  ReplacementTypeEnum_KEEP_BASE,
  ReplacementTypeEnum_KEEP_DESTINATION,
  ReplacementTypeEnum_KEEP_SOURCE,
  ReplacementTypeEnum_USE_NEW_CONTENT,
  ReplacementTypeEnum'
  #-}
