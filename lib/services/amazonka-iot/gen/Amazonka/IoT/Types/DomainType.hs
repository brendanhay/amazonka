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
-- Module      : Amazonka.IoT.Types.DomainType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DomainType
  ( DomainType
      ( ..,
        DomainType_AWS_MANAGED,
        DomainType_CUSTOMER_MANAGED,
        DomainType_ENDPOINT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DomainType = DomainType'
  { fromDomainType ::
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

pattern DomainType_AWS_MANAGED :: DomainType
pattern DomainType_AWS_MANAGED = DomainType' "AWS_MANAGED"

pattern DomainType_CUSTOMER_MANAGED :: DomainType
pattern DomainType_CUSTOMER_MANAGED = DomainType' "CUSTOMER_MANAGED"

pattern DomainType_ENDPOINT :: DomainType
pattern DomainType_ENDPOINT = DomainType' "ENDPOINT"

{-# COMPLETE
  DomainType_AWS_MANAGED,
  DomainType_CUSTOMER_MANAGED,
  DomainType_ENDPOINT,
  DomainType'
  #-}
