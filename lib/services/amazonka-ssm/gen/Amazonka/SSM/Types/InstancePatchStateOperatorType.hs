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
-- Module      : Amazonka.SSM.Types.InstancePatchStateOperatorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstancePatchStateOperatorType
  ( InstancePatchStateOperatorType
      ( ..,
        InstancePatchStateOperatorType_Equal,
        InstancePatchStateOperatorType_GreaterThan,
        InstancePatchStateOperatorType_LessThan,
        InstancePatchStateOperatorType_NotEqual
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstancePatchStateOperatorType = InstancePatchStateOperatorType'
  { fromInstancePatchStateOperatorType ::
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

pattern InstancePatchStateOperatorType_Equal :: InstancePatchStateOperatorType
pattern InstancePatchStateOperatorType_Equal = InstancePatchStateOperatorType' "Equal"

pattern InstancePatchStateOperatorType_GreaterThan :: InstancePatchStateOperatorType
pattern InstancePatchStateOperatorType_GreaterThan = InstancePatchStateOperatorType' "GreaterThan"

pattern InstancePatchStateOperatorType_LessThan :: InstancePatchStateOperatorType
pattern InstancePatchStateOperatorType_LessThan = InstancePatchStateOperatorType' "LessThan"

pattern InstancePatchStateOperatorType_NotEqual :: InstancePatchStateOperatorType
pattern InstancePatchStateOperatorType_NotEqual = InstancePatchStateOperatorType' "NotEqual"

{-# COMPLETE
  InstancePatchStateOperatorType_Equal,
  InstancePatchStateOperatorType_GreaterThan,
  InstancePatchStateOperatorType_LessThan,
  InstancePatchStateOperatorType_NotEqual,
  InstancePatchStateOperatorType'
  #-}
