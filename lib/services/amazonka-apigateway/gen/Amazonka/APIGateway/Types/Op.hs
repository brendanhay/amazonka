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
-- Module      : Amazonka.APIGateway.Types.Op
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Op
  ( Op
      ( ..,
        Op_Add,
        Op_Copy,
        Op_Move,
        Op_Remove,
        Op_Replace,
        Op_Test
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Op = Op' {fromOp :: Data.Text}
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

pattern Op_Add :: Op
pattern Op_Add = Op' "add"

pattern Op_Copy :: Op
pattern Op_Copy = Op' "copy"

pattern Op_Move :: Op
pattern Op_Move = Op' "move"

pattern Op_Remove :: Op
pattern Op_Remove = Op' "remove"

pattern Op_Replace :: Op
pattern Op_Replace = Op' "replace"

pattern Op_Test :: Op
pattern Op_Test = Op' "test"

{-# COMPLETE
  Op_Add,
  Op_Copy,
  Op_Move,
  Op_Remove,
  Op_Replace,
  Op_Test,
  Op'
  #-}
