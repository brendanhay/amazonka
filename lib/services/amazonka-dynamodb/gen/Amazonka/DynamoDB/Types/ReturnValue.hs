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
-- Module      : Amazonka.DynamoDB.Types.ReturnValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReturnValue
  ( ReturnValue
      ( ..,
        ReturnValue_ALL_NEW,
        ReturnValue_ALL_OLD,
        ReturnValue_NONE,
        ReturnValue_UPDATED_NEW,
        ReturnValue_UPDATED_OLD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype ReturnValue = ReturnValue'
  { fromReturnValue ::
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

pattern ReturnValue_ALL_NEW :: ReturnValue
pattern ReturnValue_ALL_NEW = ReturnValue' "ALL_NEW"

pattern ReturnValue_ALL_OLD :: ReturnValue
pattern ReturnValue_ALL_OLD = ReturnValue' "ALL_OLD"

pattern ReturnValue_NONE :: ReturnValue
pattern ReturnValue_NONE = ReturnValue' "NONE"

pattern ReturnValue_UPDATED_NEW :: ReturnValue
pattern ReturnValue_UPDATED_NEW = ReturnValue' "UPDATED_NEW"

pattern ReturnValue_UPDATED_OLD :: ReturnValue
pattern ReturnValue_UPDATED_OLD = ReturnValue' "UPDATED_OLD"

{-# COMPLETE
  ReturnValue_ALL_NEW,
  ReturnValue_ALL_OLD,
  ReturnValue_NONE,
  ReturnValue_UPDATED_NEW,
  ReturnValue_UPDATED_OLD,
  ReturnValue'
  #-}
