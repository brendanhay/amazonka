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
-- Module      : Amazonka.MediaConvert.Types.VchipAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VchipAction
  ( VchipAction
      ( ..,
        VchipAction_PASSTHROUGH,
        VchipAction_STRIP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The action to take on content advisory XDS packets. If you select
-- PASSTHROUGH, packets will not be changed. If you select STRIP, any
-- packets will be removed in output captions.
newtype VchipAction = VchipAction'
  { fromVchipAction ::
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

pattern VchipAction_PASSTHROUGH :: VchipAction
pattern VchipAction_PASSTHROUGH = VchipAction' "PASSTHROUGH"

pattern VchipAction_STRIP :: VchipAction
pattern VchipAction_STRIP = VchipAction' "STRIP"

{-# COMPLETE
  VchipAction_PASSTHROUGH,
  VchipAction_STRIP,
  VchipAction'
  #-}
