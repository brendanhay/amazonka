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
-- Module      : Amazonka.MediaConvert.Types.MpdTimedMetadataBoxVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MpdTimedMetadataBoxVersion
  ( MpdTimedMetadataBoxVersion
      ( ..,
        MpdTimedMetadataBoxVersion_VERSION_0,
        MpdTimedMetadataBoxVersion_VERSION_1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the event message box (eMSG) version for ID3 timed metadata in
-- your output. For more information, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.3 Syntax. Leave blank to use the default value Version 0. When
-- you specify Version 1, you must also set ID3 metadata (timedMetadata) to
-- Passthrough.
newtype MpdTimedMetadataBoxVersion = MpdTimedMetadataBoxVersion'
  { fromMpdTimedMetadataBoxVersion ::
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

pattern MpdTimedMetadataBoxVersion_VERSION_0 :: MpdTimedMetadataBoxVersion
pattern MpdTimedMetadataBoxVersion_VERSION_0 = MpdTimedMetadataBoxVersion' "VERSION_0"

pattern MpdTimedMetadataBoxVersion_VERSION_1 :: MpdTimedMetadataBoxVersion
pattern MpdTimedMetadataBoxVersion_VERSION_1 = MpdTimedMetadataBoxVersion' "VERSION_1"

{-# COMPLETE
  MpdTimedMetadataBoxVersion_VERSION_0,
  MpdTimedMetadataBoxVersion_VERSION_1,
  MpdTimedMetadataBoxVersion'
  #-}
