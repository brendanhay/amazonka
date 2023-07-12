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
-- Module      : Amazonka.MediaConvert.Types.HlsKeyProviderType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsKeyProviderType
  ( HlsKeyProviderType
      ( ..,
        HlsKeyProviderType_SPEKE,
        HlsKeyProviderType_STATIC_KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether your DRM encryption key is static or from a key provider
-- that follows the SPEKE standard. For more information about SPEKE, see
-- https:\/\/docs.aws.amazon.com\/speke\/latest\/documentation\/what-is-speke.html.
newtype HlsKeyProviderType = HlsKeyProviderType'
  { fromHlsKeyProviderType ::
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

pattern HlsKeyProviderType_SPEKE :: HlsKeyProviderType
pattern HlsKeyProviderType_SPEKE = HlsKeyProviderType' "SPEKE"

pattern HlsKeyProviderType_STATIC_KEY :: HlsKeyProviderType
pattern HlsKeyProviderType_STATIC_KEY = HlsKeyProviderType' "STATIC_KEY"

{-# COMPLETE
  HlsKeyProviderType_SPEKE,
  HlsKeyProviderType_STATIC_KEY,
  HlsKeyProviderType'
  #-}
