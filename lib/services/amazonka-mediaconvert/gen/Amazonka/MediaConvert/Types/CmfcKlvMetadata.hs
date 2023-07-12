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
-- Module      : Amazonka.MediaConvert.Types.CmfcKlvMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmfcKlvMetadata
  ( CmfcKlvMetadata
      ( ..,
        CmfcKlvMetadata_NONE,
        CmfcKlvMetadata_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | To include key-length-value metadata in this output: Set KLV metadata
-- insertion to Passthrough. MediaConvert reads KLV metadata present in
-- your input and writes each instance to a separate event message box in
-- the output, according to MISB ST1910.1. To exclude this KLV metadata:
-- Set KLV metadata insertion to None or leave blank.
newtype CmfcKlvMetadata = CmfcKlvMetadata'
  { fromCmfcKlvMetadata ::
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

pattern CmfcKlvMetadata_NONE :: CmfcKlvMetadata
pattern CmfcKlvMetadata_NONE = CmfcKlvMetadata' "NONE"

pattern CmfcKlvMetadata_PASSTHROUGH :: CmfcKlvMetadata
pattern CmfcKlvMetadata_PASSTHROUGH = CmfcKlvMetadata' "PASSTHROUGH"

{-# COMPLETE
  CmfcKlvMetadata_NONE,
  CmfcKlvMetadata_PASSTHROUGH,
  CmfcKlvMetadata'
  #-}
