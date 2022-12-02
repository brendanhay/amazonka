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
-- Module      : Amazonka.ServiceCatalog.Types.StackSetOperationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.StackSetOperationType
  ( StackSetOperationType
      ( ..,
        StackSetOperationType_CREATE,
        StackSetOperationType_DELETE,
        StackSetOperationType_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StackSetOperationType = StackSetOperationType'
  { fromStackSetOperationType ::
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
