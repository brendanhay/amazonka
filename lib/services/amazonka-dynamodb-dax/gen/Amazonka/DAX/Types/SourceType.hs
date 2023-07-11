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
-- Module      : Amazonka.DAX.Types.SourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.SourceType
  ( SourceType
      ( ..,
        SourceType_CLUSTER,
        SourceType_PARAMETER_GROUP,
        SourceType_SUBNET_GROUP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceType = SourceType'
  { fromSourceType ::
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

pattern SourceType_CLUSTER :: SourceType
pattern SourceType_CLUSTER = SourceType' "CLUSTER"

pattern SourceType_PARAMETER_GROUP :: SourceType
pattern SourceType_PARAMETER_GROUP = SourceType' "PARAMETER_GROUP"

pattern SourceType_SUBNET_GROUP :: SourceType
pattern SourceType_SUBNET_GROUP = SourceType' "SUBNET_GROUP"

{-# COMPLETE
  SourceType_CLUSTER,
  SourceType_PARAMETER_GROUP,
  SourceType_SUBNET_GROUP,
  SourceType'
  #-}
