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
-- Module      : Amazonka.Route53.Types.CidrCollectionChangeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.CidrCollectionChangeAction
  ( CidrCollectionChangeAction
      ( ..,
        CidrCollectionChangeAction_DELETE_IF_EXISTS,
        CidrCollectionChangeAction_PUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

newtype CidrCollectionChangeAction = CidrCollectionChangeAction'
  { fromCidrCollectionChangeAction ::
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

pattern CidrCollectionChangeAction_DELETE_IF_EXISTS :: CidrCollectionChangeAction
pattern CidrCollectionChangeAction_DELETE_IF_EXISTS = CidrCollectionChangeAction' "DELETE_IF_EXISTS"

pattern CidrCollectionChangeAction_PUT :: CidrCollectionChangeAction
pattern CidrCollectionChangeAction_PUT = CidrCollectionChangeAction' "PUT"

{-# COMPLETE
  CidrCollectionChangeAction_DELETE_IF_EXISTS,
  CidrCollectionChangeAction_PUT,
  CidrCollectionChangeAction'
  #-}
