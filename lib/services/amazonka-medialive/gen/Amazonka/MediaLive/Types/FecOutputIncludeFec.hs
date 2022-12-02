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
-- Module      : Amazonka.MediaLive.Types.FecOutputIncludeFec
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FecOutputIncludeFec
  ( FecOutputIncludeFec
      ( ..,
        FecOutputIncludeFec_COLUMN,
        FecOutputIncludeFec_COLUMN_AND_ROW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Fec Output Include Fec
newtype FecOutputIncludeFec = FecOutputIncludeFec'
  { fromFecOutputIncludeFec ::
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

pattern FecOutputIncludeFec_COLUMN :: FecOutputIncludeFec
pattern FecOutputIncludeFec_COLUMN = FecOutputIncludeFec' "COLUMN"

pattern FecOutputIncludeFec_COLUMN_AND_ROW :: FecOutputIncludeFec
pattern FecOutputIncludeFec_COLUMN_AND_ROW = FecOutputIncludeFec' "COLUMN_AND_ROW"

{-# COMPLETE
  FecOutputIncludeFec_COLUMN,
  FecOutputIncludeFec_COLUMN_AND_ROW,
  FecOutputIncludeFec'
  #-}
