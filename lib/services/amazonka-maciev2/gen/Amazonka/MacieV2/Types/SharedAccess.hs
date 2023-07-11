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
-- Module      : Amazonka.MacieV2.Types.SharedAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SharedAccess
  ( SharedAccess
      ( ..,
        SharedAccess_EXTERNAL,
        SharedAccess_INTERNAL,
        SharedAccess_NOT_SHARED,
        SharedAccess_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SharedAccess = SharedAccess'
  { fromSharedAccess ::
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

pattern SharedAccess_EXTERNAL :: SharedAccess
pattern SharedAccess_EXTERNAL = SharedAccess' "EXTERNAL"

pattern SharedAccess_INTERNAL :: SharedAccess
pattern SharedAccess_INTERNAL = SharedAccess' "INTERNAL"

pattern SharedAccess_NOT_SHARED :: SharedAccess
pattern SharedAccess_NOT_SHARED = SharedAccess' "NOT_SHARED"

pattern SharedAccess_UNKNOWN :: SharedAccess
pattern SharedAccess_UNKNOWN = SharedAccess' "UNKNOWN"

{-# COMPLETE
  SharedAccess_EXTERNAL,
  SharedAccess_INTERNAL,
  SharedAccess_NOT_SHARED,
  SharedAccess_UNKNOWN,
  SharedAccess'
  #-}
