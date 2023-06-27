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
-- Module      : Amazonka.ImageBuilder.Types.Ownership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Ownership
  ( Ownership
      ( ..,
        Ownership_Amazon,
        Ownership_Self,
        Ownership_Shared,
        Ownership_ThirdParty
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Ownership = Ownership'
  { fromOwnership ::
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

pattern Ownership_Amazon :: Ownership
pattern Ownership_Amazon = Ownership' "Amazon"

pattern Ownership_Self :: Ownership
pattern Ownership_Self = Ownership' "Self"

pattern Ownership_Shared :: Ownership
pattern Ownership_Shared = Ownership' "Shared"

pattern Ownership_ThirdParty :: Ownership
pattern Ownership_ThirdParty = Ownership' "ThirdParty"

{-# COMPLETE
  Ownership_Amazon,
  Ownership_Self,
  Ownership_Shared,
  Ownership_ThirdParty,
  Ownership'
  #-}
