{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FecOutputIncludeFec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FecOutputIncludeFec
  ( FecOutputIncludeFec
      ( ..,
        FecOutputIncludeFec_COLUMN,
        FecOutputIncludeFec_COLUMN_AND_ROW
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Fec Output Include Fec
newtype FecOutputIncludeFec = FecOutputIncludeFec'
  { fromFecOutputIncludeFec ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
