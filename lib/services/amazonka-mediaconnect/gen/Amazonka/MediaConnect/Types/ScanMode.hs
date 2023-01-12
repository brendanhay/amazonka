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
-- Module      : Amazonka.MediaConnect.Types.ScanMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.ScanMode
  ( ScanMode
      ( ..,
        ScanMode_Interlace,
        ScanMode_Progressive,
        ScanMode_Progressive_segmented_frame
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScanMode = ScanMode'
  { fromScanMode ::
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

pattern ScanMode_Interlace :: ScanMode
pattern ScanMode_Interlace = ScanMode' "interlace"

pattern ScanMode_Progressive :: ScanMode
pattern ScanMode_Progressive = ScanMode' "progressive"

pattern ScanMode_Progressive_segmented_frame :: ScanMode
pattern ScanMode_Progressive_segmented_frame = ScanMode' "progressive-segmented-frame"

{-# COMPLETE
  ScanMode_Interlace,
  ScanMode_Progressive,
  ScanMode_Progressive_segmented_frame,
  ScanMode'
  #-}
