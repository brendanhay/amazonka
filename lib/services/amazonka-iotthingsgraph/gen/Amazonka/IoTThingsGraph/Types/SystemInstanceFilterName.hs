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
-- Module      : Amazonka.IoTThingsGraph.Types.SystemInstanceFilterName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.SystemInstanceFilterName
  ( SystemInstanceFilterName
      ( ..,
        SystemInstanceFilterName_GREENGRASS_GROUP_NAME,
        SystemInstanceFilterName_STATUS,
        SystemInstanceFilterName_SYSTEM_TEMPLATE_ID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SystemInstanceFilterName = SystemInstanceFilterName'
  { fromSystemInstanceFilterName ::
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

pattern SystemInstanceFilterName_GREENGRASS_GROUP_NAME :: SystemInstanceFilterName
pattern SystemInstanceFilterName_GREENGRASS_GROUP_NAME = SystemInstanceFilterName' "GREENGRASS_GROUP_NAME"

pattern SystemInstanceFilterName_STATUS :: SystemInstanceFilterName
pattern SystemInstanceFilterName_STATUS = SystemInstanceFilterName' "STATUS"

pattern SystemInstanceFilterName_SYSTEM_TEMPLATE_ID :: SystemInstanceFilterName
pattern SystemInstanceFilterName_SYSTEM_TEMPLATE_ID = SystemInstanceFilterName' "SYSTEM_TEMPLATE_ID"

{-# COMPLETE
  SystemInstanceFilterName_GREENGRASS_GROUP_NAME,
  SystemInstanceFilterName_STATUS,
  SystemInstanceFilterName_SYSTEM_TEMPLATE_ID,
  SystemInstanceFilterName'
  #-}
