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
-- Module      : Amazonka.CodeCommit.Types.ChangeTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ChangeTypeEnum
  ( ChangeTypeEnum
      ( ..,
        ChangeTypeEnum_A,
        ChangeTypeEnum_D,
        ChangeTypeEnum_M
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeTypeEnum = ChangeTypeEnum'
  { fromChangeTypeEnum ::
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

pattern ChangeTypeEnum_A :: ChangeTypeEnum
pattern ChangeTypeEnum_A = ChangeTypeEnum' "A"

pattern ChangeTypeEnum_D :: ChangeTypeEnum
pattern ChangeTypeEnum_D = ChangeTypeEnum' "D"

pattern ChangeTypeEnum_M :: ChangeTypeEnum
pattern ChangeTypeEnum_M = ChangeTypeEnum' "M"

{-# COMPLETE
  ChangeTypeEnum_A,
  ChangeTypeEnum_D,
  ChangeTypeEnum_M,
  ChangeTypeEnum'
  #-}
