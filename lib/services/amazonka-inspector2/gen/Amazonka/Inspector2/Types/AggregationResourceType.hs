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
-- Module      : Amazonka.Inspector2.Types.AggregationResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AggregationResourceType
  ( AggregationResourceType
      ( ..,
        AggregationResourceType_AWS_EC2_INSTANCE,
        AggregationResourceType_AWS_ECR_CONTAINER_IMAGE,
        AggregationResourceType_AWS_LAMBDA_FUNCTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregationResourceType = AggregationResourceType'
  { fromAggregationResourceType ::
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

pattern AggregationResourceType_AWS_EC2_INSTANCE :: AggregationResourceType
pattern AggregationResourceType_AWS_EC2_INSTANCE = AggregationResourceType' "AWS_EC2_INSTANCE"

pattern AggregationResourceType_AWS_ECR_CONTAINER_IMAGE :: AggregationResourceType
pattern AggregationResourceType_AWS_ECR_CONTAINER_IMAGE = AggregationResourceType' "AWS_ECR_CONTAINER_IMAGE"

pattern AggregationResourceType_AWS_LAMBDA_FUNCTION :: AggregationResourceType
pattern AggregationResourceType_AWS_LAMBDA_FUNCTION = AggregationResourceType' "AWS_LAMBDA_FUNCTION"

{-# COMPLETE
  AggregationResourceType_AWS_EC2_INSTANCE,
  AggregationResourceType_AWS_ECR_CONTAINER_IMAGE,
  AggregationResourceType_AWS_LAMBDA_FUNCTION,
  AggregationResourceType'
  #-}
