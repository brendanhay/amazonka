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
-- Module      : Amazonka.FSx.Types.SecurityStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SecurityStyle
  ( SecurityStyle
      ( ..,
        SecurityStyle_MIXED,
        SecurityStyle_NTFS,
        SecurityStyle_UNIX
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SecurityStyle = SecurityStyle'
  { fromSecurityStyle ::
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

pattern SecurityStyle_MIXED :: SecurityStyle
pattern SecurityStyle_MIXED = SecurityStyle' "MIXED"

pattern SecurityStyle_NTFS :: SecurityStyle
pattern SecurityStyle_NTFS = SecurityStyle' "NTFS"

pattern SecurityStyle_UNIX :: SecurityStyle
pattern SecurityStyle_UNIX = SecurityStyle' "UNIX"

{-# COMPLETE
  SecurityStyle_MIXED,
  SecurityStyle_NTFS,
  SecurityStyle_UNIX,
  SecurityStyle'
  #-}
