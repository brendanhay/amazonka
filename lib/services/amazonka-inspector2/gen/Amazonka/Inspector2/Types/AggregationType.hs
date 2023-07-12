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
-- Module      : Amazonka.Inspector2.Types.AggregationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AggregationType
  ( AggregationType
      ( ..,
        AggregationType_ACCOUNT,
        AggregationType_AMI,
        AggregationType_AWS_EC2_INSTANCE,
        AggregationType_AWS_ECR_CONTAINER,
        AggregationType_AWS_LAMBDA_FUNCTION,
        AggregationType_FINDING_TYPE,
        AggregationType_IMAGE_LAYER,
        AggregationType_LAMBDA_LAYER,
        AggregationType_PACKAGE,
        AggregationType_REPOSITORY,
        AggregationType_TITLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregationType = AggregationType'
  { fromAggregationType ::
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

pattern AggregationType_ACCOUNT :: AggregationType
pattern AggregationType_ACCOUNT = AggregationType' "ACCOUNT"

pattern AggregationType_AMI :: AggregationType
pattern AggregationType_AMI = AggregationType' "AMI"

pattern AggregationType_AWS_EC2_INSTANCE :: AggregationType
pattern AggregationType_AWS_EC2_INSTANCE = AggregationType' "AWS_EC2_INSTANCE"

pattern AggregationType_AWS_ECR_CONTAINER :: AggregationType
pattern AggregationType_AWS_ECR_CONTAINER = AggregationType' "AWS_ECR_CONTAINER"

pattern AggregationType_AWS_LAMBDA_FUNCTION :: AggregationType
pattern AggregationType_AWS_LAMBDA_FUNCTION = AggregationType' "AWS_LAMBDA_FUNCTION"

pattern AggregationType_FINDING_TYPE :: AggregationType
pattern AggregationType_FINDING_TYPE = AggregationType' "FINDING_TYPE"

pattern AggregationType_IMAGE_LAYER :: AggregationType
pattern AggregationType_IMAGE_LAYER = AggregationType' "IMAGE_LAYER"

pattern AggregationType_LAMBDA_LAYER :: AggregationType
pattern AggregationType_LAMBDA_LAYER = AggregationType' "LAMBDA_LAYER"

pattern AggregationType_PACKAGE :: AggregationType
pattern AggregationType_PACKAGE = AggregationType' "PACKAGE"

pattern AggregationType_REPOSITORY :: AggregationType
pattern AggregationType_REPOSITORY = AggregationType' "REPOSITORY"

pattern AggregationType_TITLE :: AggregationType
pattern AggregationType_TITLE = AggregationType' "TITLE"

{-# COMPLETE
  AggregationType_ACCOUNT,
  AggregationType_AMI,
  AggregationType_AWS_EC2_INSTANCE,
  AggregationType_AWS_ECR_CONTAINER,
  AggregationType_AWS_LAMBDA_FUNCTION,
  AggregationType_FINDING_TYPE,
  AggregationType_IMAGE_LAYER,
  AggregationType_LAMBDA_LAYER,
  AggregationType_PACKAGE,
  AggregationType_REPOSITORY,
  AggregationType_TITLE,
  AggregationType'
  #-}
