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
-- Module      : Amazonka.RAM.Types.PermissionTypeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.PermissionTypeFilter
  ( PermissionTypeFilter
      ( ..,
        PermissionTypeFilter_ALL,
        PermissionTypeFilter_AWS_MANAGED,
        PermissionTypeFilter_CUSTOMER_MANAGED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PermissionTypeFilter = PermissionTypeFilter'
  { fromPermissionTypeFilter ::
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

pattern PermissionTypeFilter_ALL :: PermissionTypeFilter
pattern PermissionTypeFilter_ALL = PermissionTypeFilter' "ALL"

pattern PermissionTypeFilter_AWS_MANAGED :: PermissionTypeFilter
pattern PermissionTypeFilter_AWS_MANAGED = PermissionTypeFilter' "AWS_MANAGED"

pattern PermissionTypeFilter_CUSTOMER_MANAGED :: PermissionTypeFilter
pattern PermissionTypeFilter_CUSTOMER_MANAGED = PermissionTypeFilter' "CUSTOMER_MANAGED"

{-# COMPLETE
  PermissionTypeFilter_ALL,
  PermissionTypeFilter_AWS_MANAGED,
  PermissionTypeFilter_CUSTOMER_MANAGED,
  PermissionTypeFilter'
  #-}
