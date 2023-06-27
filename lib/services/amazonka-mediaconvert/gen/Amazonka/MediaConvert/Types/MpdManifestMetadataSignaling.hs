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
-- Module      : Amazonka.MediaConvert.Types.MpdManifestMetadataSignaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MpdManifestMetadataSignaling
  ( MpdManifestMetadataSignaling
      ( ..,
        MpdManifestMetadataSignaling_DISABLED,
        MpdManifestMetadataSignaling_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | To add an InbandEventStream element in your output MPD manifest for each
-- type of event message, set Manifest metadata signaling to Enabled. For
-- ID3 event messages, the InbandEventStream element schemeIdUri will be
-- same value that you specify for ID3 metadata scheme ID URI. For SCTE35
-- event messages, the InbandEventStream element schemeIdUri will be
-- \"urn:scte:scte35:2013:bin\". To leave these elements out of your output
-- MPD manifest, set Manifest metadata signaling to Disabled. To enable
-- Manifest metadata signaling, you must also set SCTE-35 source to
-- Passthrough, ESAM SCTE-35 to insert, or ID3 metadata (TimedMetadata) to
-- Passthrough.
newtype MpdManifestMetadataSignaling = MpdManifestMetadataSignaling'
  { fromMpdManifestMetadataSignaling ::
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

pattern MpdManifestMetadataSignaling_DISABLED :: MpdManifestMetadataSignaling
pattern MpdManifestMetadataSignaling_DISABLED = MpdManifestMetadataSignaling' "DISABLED"

pattern MpdManifestMetadataSignaling_ENABLED :: MpdManifestMetadataSignaling
pattern MpdManifestMetadataSignaling_ENABLED = MpdManifestMetadataSignaling' "ENABLED"

{-# COMPLETE
  MpdManifestMetadataSignaling_DISABLED,
  MpdManifestMetadataSignaling_ENABLED,
  MpdManifestMetadataSignaling'
  #-}
