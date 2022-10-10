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
-- Module      : Amazonka.Route53.Types.ChangeAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.ChangeAction
  ( ChangeAction
      ( ..,
        ChangeAction_CREATE,
        ChangeAction_DELETE,
        ChangeAction_UPSERT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

newtype ChangeAction = ChangeAction'
  { fromChangeAction ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ChangeAction_CREATE :: ChangeAction
pattern ChangeAction_CREATE = ChangeAction' "CREATE"

pattern ChangeAction_DELETE :: ChangeAction
pattern ChangeAction_DELETE = ChangeAction' "DELETE"

pattern ChangeAction_UPSERT :: ChangeAction
pattern ChangeAction_UPSERT = ChangeAction' "UPSERT"

{-# COMPLETE
  ChangeAction_CREATE,
  ChangeAction_DELETE,
  ChangeAction_UPSERT,
  ChangeAction'
  #-}
