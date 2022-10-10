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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AggregationResourceType
  ( AggregationResourceType
      ( ..,
        AggregationResourceType_AWS_EC2_INSTANCE,
        AggregationResourceType_AWS_ECR_CONTAINER_IMAGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AggregationResourceType = AggregationResourceType'
  { fromAggregationResourceType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AggregationResourceType_AWS_EC2_INSTANCE :: AggregationResourceType
pattern AggregationResourceType_AWS_EC2_INSTANCE = AggregationResourceType' "AWS_EC2_INSTANCE"

pattern AggregationResourceType_AWS_ECR_CONTAINER_IMAGE :: AggregationResourceType
pattern AggregationResourceType_AWS_ECR_CONTAINER_IMAGE = AggregationResourceType' "AWS_ECR_CONTAINER_IMAGE"

{-# COMPLETE
  AggregationResourceType_AWS_EC2_INSTANCE,
  AggregationResourceType_AWS_ECR_CONTAINER_IMAGE,
  AggregationResourceType'
  #-}
