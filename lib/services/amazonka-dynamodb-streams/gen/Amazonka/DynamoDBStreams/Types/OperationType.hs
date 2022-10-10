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
-- Module      : Amazonka.DynamoDBStreams.Types.OperationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types.OperationType
  ( OperationType
      ( ..,
        OperationType_INSERT,
        OperationType_MODIFY,
        OperationType_REMOVE
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDBStreams.Internal
import qualified Amazonka.Prelude as Prelude

newtype OperationType = OperationType'
  { fromOperationType ::
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

pattern OperationType_INSERT :: OperationType
pattern OperationType_INSERT = OperationType' "INSERT"

pattern OperationType_MODIFY :: OperationType
pattern OperationType_MODIFY = OperationType' "MODIFY"

pattern OperationType_REMOVE :: OperationType
pattern OperationType_REMOVE = OperationType' "REMOVE"

{-# COMPLETE
  OperationType_INSERT,
  OperationType_MODIFY,
  OperationType_REMOVE,
  OperationType'
  #-}
