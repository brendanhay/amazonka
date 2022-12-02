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
-- Module      : Amazonka.CloudFormation.Types.DifferenceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.DifferenceType
  ( DifferenceType
      ( ..,
        DifferenceType_ADD,
        DifferenceType_NOT_EQUAL,
        DifferenceType_REMOVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DifferenceType = DifferenceType'
  { fromDifferenceType ::
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

pattern DifferenceType_ADD :: DifferenceType
pattern DifferenceType_ADD = DifferenceType' "ADD"

pattern DifferenceType_NOT_EQUAL :: DifferenceType
pattern DifferenceType_NOT_EQUAL = DifferenceType' "NOT_EQUAL"

pattern DifferenceType_REMOVE :: DifferenceType
pattern DifferenceType_REMOVE = DifferenceType' "REMOVE"

{-# COMPLETE
  DifferenceType_ADD,
  DifferenceType_NOT_EQUAL,
  DifferenceType_REMOVE,
  DifferenceType'
  #-}
