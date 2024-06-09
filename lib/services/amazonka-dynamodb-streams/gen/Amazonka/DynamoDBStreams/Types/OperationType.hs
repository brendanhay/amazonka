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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import Amazonka.DynamoDBStreams.Types.AttributeValue
import qualified Amazonka.Prelude as Prelude

newtype OperationType = OperationType'
  { fromOperationType ::
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
