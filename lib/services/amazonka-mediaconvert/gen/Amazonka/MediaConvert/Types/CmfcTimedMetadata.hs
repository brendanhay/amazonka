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
-- Module      : Amazonka.MediaConvert.Types.CmfcTimedMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmfcTimedMetadata
  ( CmfcTimedMetadata
      ( ..,
        CmfcTimedMetadata_NONE,
        CmfcTimedMetadata_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
-- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
-- metadata inserter (timedMetadataInsertion). MediaConvert writes each
-- instance of ID3 metadata in a separate Event Message (eMSG) box. To
-- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
-- blank.
newtype CmfcTimedMetadata = CmfcTimedMetadata'
  { fromCmfcTimedMetadata ::
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

pattern CmfcTimedMetadata_NONE :: CmfcTimedMetadata
pattern CmfcTimedMetadata_NONE = CmfcTimedMetadata' "NONE"

pattern CmfcTimedMetadata_PASSTHROUGH :: CmfcTimedMetadata
pattern CmfcTimedMetadata_PASSTHROUGH = CmfcTimedMetadata' "PASSTHROUGH"

{-# COMPLETE
  CmfcTimedMetadata_NONE,
  CmfcTimedMetadata_PASSTHROUGH,
  CmfcTimedMetadata'
  #-}
