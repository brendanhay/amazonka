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
-- Module      : Amazonka.AppStream.Types.VisibilityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.VisibilityType
  ( VisibilityType
      ( ..,
        VisibilityType_PRIVATE,
        VisibilityType_PUBLIC,
        VisibilityType_SHARED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VisibilityType = VisibilityType'
  { fromVisibilityType ::
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

pattern VisibilityType_PRIVATE :: VisibilityType
pattern VisibilityType_PRIVATE = VisibilityType' "PRIVATE"

pattern VisibilityType_PUBLIC :: VisibilityType
pattern VisibilityType_PUBLIC = VisibilityType' "PUBLIC"

pattern VisibilityType_SHARED :: VisibilityType
pattern VisibilityType_SHARED = VisibilityType' "SHARED"

{-# COMPLETE
  VisibilityType_PRIVATE,
  VisibilityType_PUBLIC,
  VisibilityType_SHARED,
  VisibilityType'
  #-}
