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
-- Module      : Amazonka.Glue.Types.HudiTargetCompressionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.HudiTargetCompressionType
  ( HudiTargetCompressionType
      ( ..,
        HudiTargetCompressionType_Gzip,
        HudiTargetCompressionType_Lzo,
        HudiTargetCompressionType_Snappy,
        HudiTargetCompressionType_Uncompressed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HudiTargetCompressionType = HudiTargetCompressionType'
  { fromHudiTargetCompressionType ::
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

pattern HudiTargetCompressionType_Gzip :: HudiTargetCompressionType
pattern HudiTargetCompressionType_Gzip = HudiTargetCompressionType' "gzip"

pattern HudiTargetCompressionType_Lzo :: HudiTargetCompressionType
pattern HudiTargetCompressionType_Lzo = HudiTargetCompressionType' "lzo"

pattern HudiTargetCompressionType_Snappy :: HudiTargetCompressionType
pattern HudiTargetCompressionType_Snappy = HudiTargetCompressionType' "snappy"

pattern HudiTargetCompressionType_Uncompressed :: HudiTargetCompressionType
pattern HudiTargetCompressionType_Uncompressed = HudiTargetCompressionType' "uncompressed"

{-# COMPLETE
  HudiTargetCompressionType_Gzip,
  HudiTargetCompressionType_Lzo,
  HudiTargetCompressionType_Snappy,
  HudiTargetCompressionType_Uncompressed,
  HudiTargetCompressionType'
  #-}
