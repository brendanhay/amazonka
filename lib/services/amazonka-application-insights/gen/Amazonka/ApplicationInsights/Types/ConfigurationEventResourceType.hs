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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigurationEventResourceType = ConfigurationEventResourceType'
  { fromConfigurationEventResourceType ::
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
