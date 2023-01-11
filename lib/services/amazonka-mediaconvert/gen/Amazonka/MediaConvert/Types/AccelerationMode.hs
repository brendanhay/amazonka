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
-- Module      : Amazonka.MediaConvert.Types.AccelerationMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AccelerationMode
  ( AccelerationMode
      ( ..,
        AccelerationMode_DISABLED,
        AccelerationMode_ENABLED,
        AccelerationMode_PREFERRED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether the service runs your job with accelerated transcoding.
-- Choose DISABLED if you don\'t want accelerated transcoding. Choose
-- ENABLED if you want your job to run with accelerated transcoding and to
-- fail if your input files or your job settings aren\'t compatible with
-- accelerated transcoding. Choose PREFERRED if you want your job to run
-- with accelerated transcoding if the job is compatible with the feature
-- and to run at standard speed if it\'s not.
newtype AccelerationMode = AccelerationMode'
  { fromAccelerationMode ::
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

pattern AccelerationMode_DISABLED :: AccelerationMode
pattern AccelerationMode_DISABLED = AccelerationMode' "DISABLED"

pattern AccelerationMode_ENABLED :: AccelerationMode
pattern AccelerationMode_ENABLED = AccelerationMode' "ENABLED"

pattern AccelerationMode_PREFERRED :: AccelerationMode
pattern AccelerationMode_PREFERRED = AccelerationMode' "PREFERRED"

{-# COMPLETE
  AccelerationMode_DISABLED,
  AccelerationMode_ENABLED,
  AccelerationMode_PREFERRED,
  AccelerationMode'
  #-}
