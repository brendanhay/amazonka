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
import qualified Amazonka.Prelude as Prelude

newtype AwsManagedTargetDestination = AwsManagedTargetDestination'
  { fromAwsManagedTargetDestination ::
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
