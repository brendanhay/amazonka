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
-- Module      : Amazonka.Transfer.Types.SigningAlg
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.SigningAlg
  ( SigningAlg
      ( ..,
        SigningAlg_NONE,
        SigningAlg_SHA1,
        SigningAlg_SHA256,
        SigningAlg_SHA384,
        SigningAlg_SHA512
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SigningAlg = SigningAlg'
  { fromSigningAlg ::
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

pattern SigningAlg_NONE :: SigningAlg
pattern SigningAlg_NONE = SigningAlg' "NONE"

pattern SigningAlg_SHA1 :: SigningAlg
pattern SigningAlg_SHA1 = SigningAlg' "SHA1"

pattern SigningAlg_SHA256 :: SigningAlg
pattern SigningAlg_SHA256 = SigningAlg' "SHA256"

pattern SigningAlg_SHA384 :: SigningAlg
pattern SigningAlg_SHA384 = SigningAlg' "SHA384"

pattern SigningAlg_SHA512 :: SigningAlg
pattern SigningAlg_SHA512 = SigningAlg' "SHA512"

{-# COMPLETE
  SigningAlg_NONE,
  SigningAlg_SHA1,
  SigningAlg_SHA256,
  SigningAlg_SHA384,
  SigningAlg_SHA512,
  SigningAlg'
  #-}
