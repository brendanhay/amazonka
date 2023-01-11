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
-- Module      : Amazonka.MediaLive.Types.H264ForceFieldPictures
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264ForceFieldPictures
  ( H264ForceFieldPictures
      ( ..,
        H264ForceFieldPictures_DISABLED,
        H264ForceFieldPictures_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H264 Force Field Pictures
newtype H264ForceFieldPictures = H264ForceFieldPictures'
  { fromH264ForceFieldPictures ::
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

pattern H264ForceFieldPictures_DISABLED :: H264ForceFieldPictures
pattern H264ForceFieldPictures_DISABLED = H264ForceFieldPictures' "DISABLED"

pattern H264ForceFieldPictures_ENABLED :: H264ForceFieldPictures
pattern H264ForceFieldPictures_ENABLED = H264ForceFieldPictures' "ENABLED"

{-# COMPLETE
  H264ForceFieldPictures_DISABLED,
  H264ForceFieldPictures_ENABLED,
  H264ForceFieldPictures'
  #-}
