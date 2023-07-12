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
-- Module      : Amazonka.MediaLive.Types.H265RateControlMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H265RateControlMode
  ( H265RateControlMode
      ( ..,
        H265RateControlMode_CBR,
        H265RateControlMode_MULTIPLEX,
        H265RateControlMode_QVBR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H265 Rate Control Mode
newtype H265RateControlMode = H265RateControlMode'
  { fromH265RateControlMode ::
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
