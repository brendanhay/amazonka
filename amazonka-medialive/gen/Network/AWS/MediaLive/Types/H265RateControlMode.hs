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
-- Module      : Network.AWS.MediaLive.Types.H265RateControlMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265RateControlMode
  ( H265RateControlMode
      ( ..,
        H265RateControlMode_CBR,
        H265RateControlMode_MULTIPLEX,
        H265RateControlMode_QVBR
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | H265 Rate Control Mode
newtype H265RateControlMode = H265RateControlMode'
  { fromH265RateControlMode ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern H265RateControlMode_CBR :: H265RateControlMode
pattern H265RateControlMode_CBR = H265RateControlMode' "CBR"

pattern H265RateControlMode_MULTIPLEX :: H265RateControlMode
pattern H265RateControlMode_MULTIPLEX = H265RateControlMode' "MULTIPLEX"

pattern H265RateControlMode_QVBR :: H265RateControlMode
pattern H265RateControlMode_QVBR = H265RateControlMode' "QVBR"

{-# COMPLETE
  H265RateControlMode_CBR,
  H265RateControlMode_MULTIPLEX,
  H265RateControlMode_QVBR,
  H265RateControlMode'
  #-}
