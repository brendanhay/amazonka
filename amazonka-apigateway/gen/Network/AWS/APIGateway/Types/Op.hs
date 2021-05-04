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
-- Module      : Network.AWS.APIGateway.Types.Op
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Op
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

import qualified Network.AWS.Prelude as Prelude

newtype Op = Op' {fromOp :: Prelude.Text}
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
