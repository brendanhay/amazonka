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
-- Module      : Amazonka.MediaLive.Types.TtmlDestinationStyleControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TtmlDestinationStyleControl
  ( TtmlDestinationStyleControl
      ( ..,
        TtmlDestinationStyleControl_PASSTHROUGH,
        TtmlDestinationStyleControl_USE_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ttml Destination Style Control
newtype TtmlDestinationStyleControl = TtmlDestinationStyleControl'
  { fromTtmlDestinationStyleControl ::
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

pattern TtmlDestinationStyleControl_PASSTHROUGH :: TtmlDestinationStyleControl
pattern TtmlDestinationStyleControl_PASSTHROUGH = TtmlDestinationStyleControl' "PASSTHROUGH"

pattern TtmlDestinationStyleControl_USE_CONFIGURED :: TtmlDestinationStyleControl
pattern TtmlDestinationStyleControl_USE_CONFIGURED = TtmlDestinationStyleControl' "USE_CONFIGURED"

{-# COMPLETE
  TtmlDestinationStyleControl_PASSTHROUGH,
  TtmlDestinationStyleControl_USE_CONFIGURED,
  TtmlDestinationStyleControl'
  #-}
