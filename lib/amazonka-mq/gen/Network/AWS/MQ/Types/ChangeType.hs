{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ChangeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.ChangeType
  ( ChangeType
      ( ChangeType',
        ChangeTypeCreate,
        ChangeTypeUpdate,
        ChangeTypeDelete,
        fromChangeType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The type of change pending for the ActiveMQ user.
newtype ChangeType = ChangeType' {fromChangeType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ChangeTypeCreate :: ChangeType
pattern ChangeTypeCreate = ChangeType' "CREATE"

pattern ChangeTypeUpdate :: ChangeType
pattern ChangeTypeUpdate = ChangeType' "UPDATE"

pattern ChangeTypeDelete :: ChangeType
pattern ChangeTypeDelete = ChangeType' "DELETE"

{-# COMPLETE
  ChangeTypeCreate,
  ChangeTypeUpdate,
  ChangeTypeDelete,
  ChangeType'
  #-}
