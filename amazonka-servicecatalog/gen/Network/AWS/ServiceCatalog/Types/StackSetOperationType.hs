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
-- Module      : Network.AWS.ServiceCatalog.Types.StackSetOperationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.StackSetOperationType
  ( StackSetOperationType
      ( ..,
        StackSetOperationType_CREATE,
        StackSetOperationType_DELETE,
        StackSetOperationType_UPDATE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StackSetOperationType = StackSetOperationType'
  { fromStackSetOperationType ::
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

pattern StackSetOperationType_CREATE :: StackSetOperationType
pattern StackSetOperationType_CREATE = StackSetOperationType' "CREATE"

pattern StackSetOperationType_DELETE :: StackSetOperationType
pattern StackSetOperationType_DELETE = StackSetOperationType' "DELETE"

pattern StackSetOperationType_UPDATE :: StackSetOperationType
pattern StackSetOperationType_UPDATE = StackSetOperationType' "UPDATE"

{-# COMPLETE
  StackSetOperationType_CREATE,
  StackSetOperationType_DELETE,
  StackSetOperationType_UPDATE,
  StackSetOperationType'
  #-}
