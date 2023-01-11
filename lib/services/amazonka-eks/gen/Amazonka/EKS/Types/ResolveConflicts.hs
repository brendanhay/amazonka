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
-- Module      : Amazonka.EKS.Types.ResolveConflicts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ResolveConflicts
  ( ResolveConflicts
      ( ..,
        ResolveConflicts_NONE,
        ResolveConflicts_OVERWRITE,
        ResolveConflicts_PRESERVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResolveConflicts = ResolveConflicts'
  { fromResolveConflicts ::
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

pattern ResolveConflicts_NONE :: ResolveConflicts
pattern ResolveConflicts_NONE = ResolveConflicts' "NONE"

pattern ResolveConflicts_OVERWRITE :: ResolveConflicts
pattern ResolveConflicts_OVERWRITE = ResolveConflicts' "OVERWRITE"

pattern ResolveConflicts_PRESERVE :: ResolveConflicts
pattern ResolveConflicts_PRESERVE = ResolveConflicts' "PRESERVE"

{-# COMPLETE
  ResolveConflicts_NONE,
  ResolveConflicts_OVERWRITE,
  ResolveConflicts_PRESERVE,
  ResolveConflicts'
  #-}
