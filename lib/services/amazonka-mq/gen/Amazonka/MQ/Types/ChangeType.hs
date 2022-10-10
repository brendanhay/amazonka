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
-- Module      : Amazonka.MQ.Types.ChangeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.ChangeType
  ( ChangeType
      ( ..,
        ChangeType_CREATE,
        ChangeType_DELETE,
        ChangeType_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The type of change pending for the ActiveMQ user.
newtype ChangeType = ChangeType'
  { fromChangeType ::
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

pattern ChangeType_CREATE :: ChangeType
pattern ChangeType_CREATE = ChangeType' "CREATE"

pattern ChangeType_DELETE :: ChangeType
pattern ChangeType_DELETE = ChangeType' "DELETE"

pattern ChangeType_UPDATE :: ChangeType
pattern ChangeType_UPDATE = ChangeType' "UPDATE"

{-# COMPLETE
  ChangeType_CREATE,
  ChangeType_DELETE,
  ChangeType_UPDATE,
  ChangeType'
  #-}
