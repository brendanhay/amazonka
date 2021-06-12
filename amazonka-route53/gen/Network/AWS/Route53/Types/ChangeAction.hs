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
-- Module      : Network.AWS.Route53.Types.ChangeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeAction
  ( ChangeAction
      ( ..,
        ChangeAction_CREATE,
        ChangeAction_DELETE,
        ChangeAction_UPSERT
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Route53.Internal

newtype ChangeAction = ChangeAction'
  { fromChangeAction ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
