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
-- Module      : Amazonka.Inspector2.Types.RepositorySortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.RepositorySortBy
  ( RepositorySortBy
      ( ..,
        RepositorySortBy_AFFECTED_IMAGES,
        RepositorySortBy_ALL,
        RepositorySortBy_CRITICAL,
        RepositorySortBy_HIGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RepositorySortBy = RepositorySortBy'
  { fromRepositorySortBy ::
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

pattern RepositorySortBy_AFFECTED_IMAGES :: RepositorySortBy
pattern RepositorySortBy_AFFECTED_IMAGES = RepositorySortBy' "AFFECTED_IMAGES"

pattern RepositorySortBy_ALL :: RepositorySortBy
pattern RepositorySortBy_ALL = RepositorySortBy' "ALL"

pattern RepositorySortBy_CRITICAL :: RepositorySortBy
pattern RepositorySortBy_CRITICAL = RepositorySortBy' "CRITICAL"

pattern RepositorySortBy_HIGH :: RepositorySortBy
pattern RepositorySortBy_HIGH = RepositorySortBy' "HIGH"

{-# COMPLETE
  RepositorySortBy_AFFECTED_IMAGES,
  RepositorySortBy_ALL,
  RepositorySortBy_CRITICAL,
  RepositorySortBy_HIGH,
  RepositorySortBy'
  #-}
