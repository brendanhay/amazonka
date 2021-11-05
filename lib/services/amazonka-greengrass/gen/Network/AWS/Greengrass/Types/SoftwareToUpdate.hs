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
-- Module      : Amazonka.Greengrass.Types.SoftwareToUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.SoftwareToUpdate
  ( SoftwareToUpdate
      ( ..,
        SoftwareToUpdate_Core,
        SoftwareToUpdate_Ota_agent
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The piece of software on the Greengrass core that will be updated.
newtype SoftwareToUpdate = SoftwareToUpdate'
  { fromSoftwareToUpdate ::
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

pattern SoftwareToUpdate_Core :: SoftwareToUpdate
pattern SoftwareToUpdate_Core = SoftwareToUpdate' "core"

pattern SoftwareToUpdate_Ota_agent :: SoftwareToUpdate
pattern SoftwareToUpdate_Ota_agent = SoftwareToUpdate' "ota_agent"

{-# COMPLETE
  SoftwareToUpdate_Core,
  SoftwareToUpdate_Ota_agent,
  SoftwareToUpdate'
  #-}
