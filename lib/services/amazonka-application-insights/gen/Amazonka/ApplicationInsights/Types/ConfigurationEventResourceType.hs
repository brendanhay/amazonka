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
-- Module      : Amazonka.ApplicationInsights.Types.ConfigurationEventResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.ConfigurationEventResourceType
  ( ConfigurationEventResourceType
      ( ..,
        ConfigurationEventResourceType_CLOUDFORMATION,
        ConfigurationEventResourceType_CLOUDWATCH_ALARM,
        ConfigurationEventResourceType_CLOUDWATCH_LOG,
        ConfigurationEventResourceType_SSM_ASSOCIATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ConfigurationEventResourceType = ConfigurationEventResourceType'
  { fromConfigurationEventResourceType ::
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

pattern ConfigurationEventResourceType_CLOUDFORMATION :: ConfigurationEventResourceType
pattern ConfigurationEventResourceType_CLOUDFORMATION = ConfigurationEventResourceType' "CLOUDFORMATION"

pattern ConfigurationEventResourceType_CLOUDWATCH_ALARM :: ConfigurationEventResourceType
pattern ConfigurationEventResourceType_CLOUDWATCH_ALARM = ConfigurationEventResourceType' "CLOUDWATCH_ALARM"

pattern ConfigurationEventResourceType_CLOUDWATCH_LOG :: ConfigurationEventResourceType
pattern ConfigurationEventResourceType_CLOUDWATCH_LOG = ConfigurationEventResourceType' "CLOUDWATCH_LOG"

pattern ConfigurationEventResourceType_SSM_ASSOCIATION :: ConfigurationEventResourceType
pattern ConfigurationEventResourceType_SSM_ASSOCIATION = ConfigurationEventResourceType' "SSM_ASSOCIATION"

{-# COMPLETE
  ConfigurationEventResourceType_CLOUDFORMATION,
  ConfigurationEventResourceType_CLOUDWATCH_ALARM,
  ConfigurationEventResourceType_CLOUDWATCH_LOG,
  ConfigurationEventResourceType_SSM_ASSOCIATION,
  ConfigurationEventResourceType'
  #-}
