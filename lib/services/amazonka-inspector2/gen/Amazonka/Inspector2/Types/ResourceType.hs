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
-- Module      : Amazonka.Inspector2.Types.ResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_AWS_EC2_INSTANCE,
        ResourceType_AWS_ECR_CONTAINER_IMAGE,
        ResourceType_AWS_ECR_REPOSITORY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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

pattern ResourceType_AWS_EC2_INSTANCE :: ResourceType
pattern ResourceType_AWS_EC2_INSTANCE = ResourceType' "AWS_EC2_INSTANCE"

pattern ResourceType_AWS_ECR_CONTAINER_IMAGE :: ResourceType
pattern ResourceType_AWS_ECR_CONTAINER_IMAGE = ResourceType' "AWS_ECR_CONTAINER_IMAGE"

pattern ResourceType_AWS_ECR_REPOSITORY :: ResourceType
pattern ResourceType_AWS_ECR_REPOSITORY = ResourceType' "AWS_ECR_REPOSITORY"

{-# COMPLETE
  ResourceType_AWS_EC2_INSTANCE,
  ResourceType_AWS_ECR_CONTAINER_IMAGE,
  ResourceType_AWS_ECR_REPOSITORY,
  ResourceType'
  #-}
