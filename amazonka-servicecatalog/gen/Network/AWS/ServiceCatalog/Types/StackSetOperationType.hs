{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype StackSetOperationType = StackSetOperationType'
  { fromStackSetOperationType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
