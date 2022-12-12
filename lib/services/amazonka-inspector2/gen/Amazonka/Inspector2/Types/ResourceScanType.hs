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
-- Module      : Amazonka.Inspector2.Types.ResourceScanType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceScanType
  ( ResourceScanType
      ( ..,
        ResourceScanType_EC2,
        ResourceScanType_ECR,
        ResourceScanType_LAMBDA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceScanType = ResourceScanType'
  { fromResourceScanType ::
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

pattern ResourceScanType_EC2 :: ResourceScanType
pattern ResourceScanType_EC2 = ResourceScanType' "EC2"

pattern ResourceScanType_ECR :: ResourceScanType
pattern ResourceScanType_ECR = ResourceScanType' "ECR"

pattern ResourceScanType_LAMBDA :: ResourceScanType
pattern ResourceScanType_LAMBDA = ResourceScanType' "LAMBDA"

{-# COMPLETE
  ResourceScanType_EC2,
  ResourceScanType_ECR,
  ResourceScanType_LAMBDA,
  ResourceScanType'
  #-}
