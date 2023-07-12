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
-- Module      : Amazonka.MediaConvert.Types.H265UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265UnregisteredSeiTimecode
  ( H265UnregisteredSeiTimecode
      ( ..,
        H265UnregisteredSeiTimecode_DISABLED,
        H265UnregisteredSeiTimecode_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI
-- message.
newtype H265UnregisteredSeiTimecode = H265UnregisteredSeiTimecode'
  { fromH265UnregisteredSeiTimecode ::
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

pattern H265UnregisteredSeiTimecode_DISABLED :: H265UnregisteredSeiTimecode
pattern H265UnregisteredSeiTimecode_DISABLED = H265UnregisteredSeiTimecode' "DISABLED"

pattern H265UnregisteredSeiTimecode_ENABLED :: H265UnregisteredSeiTimecode
pattern H265UnregisteredSeiTimecode_ENABLED = H265UnregisteredSeiTimecode' "ENABLED"

{-# COMPLETE
  H265UnregisteredSeiTimecode_DISABLED,
  H265UnregisteredSeiTimecode_ENABLED,
  H265UnregisteredSeiTimecode'
  #-}
