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
-- Module      : Amazonka.MediaLive.Types.Ac3MetadataControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Ac3MetadataControl
  ( Ac3MetadataControl
      ( ..,
        Ac3MetadataControl_FOLLOW_INPUT,
        Ac3MetadataControl_USE_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ac3 Metadata Control
newtype Ac3MetadataControl = Ac3MetadataControl'
  { fromAc3MetadataControl ::
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

pattern Ac3MetadataControl_FOLLOW_INPUT :: Ac3MetadataControl
pattern Ac3MetadataControl_FOLLOW_INPUT = Ac3MetadataControl' "FOLLOW_INPUT"

pattern Ac3MetadataControl_USE_CONFIGURED :: Ac3MetadataControl
pattern Ac3MetadataControl_USE_CONFIGURED = Ac3MetadataControl' "USE_CONFIGURED"

{-# COMPLETE
  Ac3MetadataControl_FOLLOW_INPUT,
  Ac3MetadataControl_USE_CONFIGURED,
  Ac3MetadataControl'
  #-}
