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
-- Module      : Amazonka.Transfer.Types.MdnSigningAlg
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.MdnSigningAlg
  ( MdnSigningAlg
      ( ..,
        MdnSigningAlg_DEFAULT,
        MdnSigningAlg_NONE,
        MdnSigningAlg_SHA1,
        MdnSigningAlg_SHA256,
        MdnSigningAlg_SHA384,
        MdnSigningAlg_SHA512
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MdnSigningAlg = MdnSigningAlg'
  { fromMdnSigningAlg ::
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

pattern MdnSigningAlg_DEFAULT :: MdnSigningAlg
pattern MdnSigningAlg_DEFAULT = MdnSigningAlg' "DEFAULT"

pattern MdnSigningAlg_NONE :: MdnSigningAlg
pattern MdnSigningAlg_NONE = MdnSigningAlg' "NONE"

pattern MdnSigningAlg_SHA1 :: MdnSigningAlg
pattern MdnSigningAlg_SHA1 = MdnSigningAlg' "SHA1"

pattern MdnSigningAlg_SHA256 :: MdnSigningAlg
pattern MdnSigningAlg_SHA256 = MdnSigningAlg' "SHA256"

pattern MdnSigningAlg_SHA384 :: MdnSigningAlg
pattern MdnSigningAlg_SHA384 = MdnSigningAlg' "SHA384"

pattern MdnSigningAlg_SHA512 :: MdnSigningAlg
pattern MdnSigningAlg_SHA512 = MdnSigningAlg' "SHA512"

{-# COMPLETE
  MdnSigningAlg_DEFAULT,
  MdnSigningAlg_NONE,
  MdnSigningAlg_SHA1,
  MdnSigningAlg_SHA256,
  MdnSigningAlg_SHA384,
  MdnSigningAlg_SHA512,
  MdnSigningAlg'
  #-}
