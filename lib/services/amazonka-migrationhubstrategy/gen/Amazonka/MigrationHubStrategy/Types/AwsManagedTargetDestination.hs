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
-- Module      : Amazonka.MigrationHubStrategy.Types.AwsManagedTargetDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AwsManagedTargetDestination
  ( AwsManagedTargetDestination
      ( ..,
        AwsManagedTargetDestination_AWS_Elastic_BeanStalk,
        AwsManagedTargetDestination_AWS_Fargate,
        AwsManagedTargetDestination_None_specified
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AwsManagedTargetDestination = AwsManagedTargetDestination'
  { fromAwsManagedTargetDestination ::
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

pattern AwsManagedTargetDestination_AWS_Elastic_BeanStalk :: AwsManagedTargetDestination
pattern AwsManagedTargetDestination_AWS_Elastic_BeanStalk = AwsManagedTargetDestination' "AWS Elastic BeanStalk"

pattern AwsManagedTargetDestination_AWS_Fargate :: AwsManagedTargetDestination
pattern AwsManagedTargetDestination_AWS_Fargate = AwsManagedTargetDestination' "AWS Fargate"

pattern AwsManagedTargetDestination_None_specified :: AwsManagedTargetDestination
pattern AwsManagedTargetDestination_None_specified = AwsManagedTargetDestination' "None specified"

{-# COMPLETE
  AwsManagedTargetDestination_AWS_Elastic_BeanStalk,
  AwsManagedTargetDestination_AWS_Fargate,
  AwsManagedTargetDestination_None_specified,
  AwsManagedTargetDestination'
  #-}
