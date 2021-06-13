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
-- Module      : Network.AWS.MediaLive.Types.Eac3MetadataControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3MetadataControl
  ( Eac3MetadataControl
      ( ..,
        Eac3MetadataControl_FOLLOW_INPUT,
        Eac3MetadataControl_USE_CONFIGURED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Eac3 Metadata Control
newtype Eac3MetadataControl = Eac3MetadataControl'
  { fromEac3MetadataControl ::
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

pattern Eac3MetadataControl_FOLLOW_INPUT :: Eac3MetadataControl
pattern Eac3MetadataControl_FOLLOW_INPUT = Eac3MetadataControl' "FOLLOW_INPUT"

pattern Eac3MetadataControl_USE_CONFIGURED :: Eac3MetadataControl
pattern Eac3MetadataControl_USE_CONFIGURED = Eac3MetadataControl' "USE_CONFIGURED"

{-# COMPLETE
  Eac3MetadataControl_FOLLOW_INPUT,
  Eac3MetadataControl_USE_CONFIGURED,
  Eac3MetadataControl'
  #-}
