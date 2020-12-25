{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3MetadataControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3MetadataControl
  ( Eac3MetadataControl
      ( Eac3MetadataControl',
        Eac3MetadataControlFollowInput,
        Eac3MetadataControlUseConfigured,
        fromEac3MetadataControl
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
newtype Eac3MetadataControl = Eac3MetadataControl'
  { fromEac3MetadataControl ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Eac3MetadataControlFollowInput :: Eac3MetadataControl
pattern Eac3MetadataControlFollowInput = Eac3MetadataControl' "FOLLOW_INPUT"

pattern Eac3MetadataControlUseConfigured :: Eac3MetadataControl
pattern Eac3MetadataControlUseConfigured = Eac3MetadataControl' "USE_CONFIGURED"

{-# COMPLETE
  Eac3MetadataControlFollowInput,
  Eac3MetadataControlUseConfigured,
  Eac3MetadataControl'
  #-}
