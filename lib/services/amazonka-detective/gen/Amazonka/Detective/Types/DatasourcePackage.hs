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
-- Module      : Amazonka.Detective.Types.DatasourcePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.DatasourcePackage
  ( DatasourcePackage
      ( ..,
        DatasourcePackage_DETECTIVE_CORE,
        DatasourcePackage_EKS_AUDIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatasourcePackage = DatasourcePackage'
  { fromDatasourcePackage ::
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

pattern DatasourcePackage_DETECTIVE_CORE :: DatasourcePackage
pattern DatasourcePackage_DETECTIVE_CORE = DatasourcePackage' "DETECTIVE_CORE"

pattern DatasourcePackage_EKS_AUDIT :: DatasourcePackage
pattern DatasourcePackage_EKS_AUDIT = DatasourcePackage' "EKS_AUDIT"

{-# COMPLETE
  DatasourcePackage_DETECTIVE_CORE,
  DatasourcePackage_EKS_AUDIT,
  DatasourcePackage'
  #-}
