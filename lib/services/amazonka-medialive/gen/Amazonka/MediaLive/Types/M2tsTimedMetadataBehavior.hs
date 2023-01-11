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
-- Module      : Amazonka.MediaLive.Types.M2tsTimedMetadataBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M2tsTimedMetadataBehavior
  ( M2tsTimedMetadataBehavior
      ( ..,
        M2tsTimedMetadataBehavior_NO_PASSTHROUGH,
        M2tsTimedMetadataBehavior_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | M2ts Timed Metadata Behavior
newtype M2tsTimedMetadataBehavior = M2tsTimedMetadataBehavior'
  { fromM2tsTimedMetadataBehavior ::
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

pattern M2tsTimedMetadataBehavior_NO_PASSTHROUGH :: M2tsTimedMetadataBehavior
pattern M2tsTimedMetadataBehavior_NO_PASSTHROUGH = M2tsTimedMetadataBehavior' "NO_PASSTHROUGH"

pattern M2tsTimedMetadataBehavior_PASSTHROUGH :: M2tsTimedMetadataBehavior
pattern M2tsTimedMetadataBehavior_PASSTHROUGH = M2tsTimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  M2tsTimedMetadataBehavior_NO_PASSTHROUGH,
  M2tsTimedMetadataBehavior_PASSTHROUGH,
  M2tsTimedMetadataBehavior'
  #-}
