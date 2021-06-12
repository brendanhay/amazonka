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
-- Module      : Network.AWS.MQ.Types.ChangeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.ChangeType
  ( ChangeType
      ( ..,
        ChangeType_CREATE,
        ChangeType_DELETE,
        ChangeType_UPDATE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The type of change pending for the ActiveMQ user.
newtype ChangeType = ChangeType'
  { fromChangeType ::
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
