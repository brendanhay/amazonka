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
-- Module      : Amazonka.StorageGateway.Types.CaseSensitivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.CaseSensitivity
  ( CaseSensitivity
      ( ..,
        CaseSensitivity_CaseSensitive,
        CaseSensitivity_ClientSpecified
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CaseSensitivity = CaseSensitivity'
  { fromCaseSensitivity ::
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

pattern CaseSensitivity_CaseSensitive :: CaseSensitivity
pattern CaseSensitivity_CaseSensitive = CaseSensitivity' "CaseSensitive"

pattern CaseSensitivity_ClientSpecified :: CaseSensitivity
pattern CaseSensitivity_ClientSpecified = CaseSensitivity' "ClientSpecified"

{-# COMPLETE
  CaseSensitivity_CaseSensitive,
  CaseSensitivity_ClientSpecified,
  CaseSensitivity'
  #-}
