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
-- Module      : Amazonka.MediaConvert.Types.AfdSignaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AfdSignaling
  ( AfdSignaling
      ( ..,
        AfdSignaling_AUTO,
        AfdSignaling_FIXED,
        AfdSignaling_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
-- AFD signaling (AfdSignaling) to specify whether the service includes AFD
-- values in the output video data and what those values are. * Choose None
-- to remove all AFD values from this output. * Choose Fixed to ignore
-- input AFD values and instead encode the value specified in the job. *
-- Choose Auto to calculate output AFD values based on the input AFD scaler
-- data.
newtype AfdSignaling = AfdSignaling'
  { fromAfdSignaling ::
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

pattern AfdSignaling_AUTO :: AfdSignaling
pattern AfdSignaling_AUTO = AfdSignaling' "AUTO"

pattern AfdSignaling_FIXED :: AfdSignaling
pattern AfdSignaling_FIXED = AfdSignaling' "FIXED"

pattern AfdSignaling_NONE :: AfdSignaling
pattern AfdSignaling_NONE = AfdSignaling' "NONE"

{-# COMPLETE
  AfdSignaling_AUTO,
  AfdSignaling_FIXED,
  AfdSignaling_NONE,
  AfdSignaling'
  #-}
