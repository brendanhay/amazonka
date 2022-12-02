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
-- Module      : Amazonka.Glue.Types.ExistCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ExistCondition
  ( ExistCondition
      ( ..,
        ExistCondition_MUST_EXIST,
        ExistCondition_NONE,
        ExistCondition_NOT_EXIST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExistCondition = ExistCondition'
  { fromExistCondition ::
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

pattern ExistCondition_MUST_EXIST :: ExistCondition
pattern ExistCondition_MUST_EXIST = ExistCondition' "MUST_EXIST"

pattern ExistCondition_NONE :: ExistCondition
pattern ExistCondition_NONE = ExistCondition' "NONE"

pattern ExistCondition_NOT_EXIST :: ExistCondition
pattern ExistCondition_NOT_EXIST = ExistCondition' "NOT_EXIST"

{-# COMPLETE
  ExistCondition_MUST_EXIST,
  ExistCondition_NONE,
  ExistCondition_NOT_EXIST,
  ExistCondition'
  #-}
