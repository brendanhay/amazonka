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
-- Module      : Amazonka.DirectoryService.Types.RadiusAuthenticationProtocol
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.RadiusAuthenticationProtocol
  ( RadiusAuthenticationProtocol
      ( ..,
        RadiusAuthenticationProtocol_CHAP,
        RadiusAuthenticationProtocol_MS_CHAPv1,
        RadiusAuthenticationProtocol_MS_CHAPv2,
        RadiusAuthenticationProtocol_PAP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RadiusAuthenticationProtocol = RadiusAuthenticationProtocol'
  { fromRadiusAuthenticationProtocol ::
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

pattern RadiusAuthenticationProtocol_CHAP :: RadiusAuthenticationProtocol
pattern RadiusAuthenticationProtocol_CHAP = RadiusAuthenticationProtocol' "CHAP"

pattern RadiusAuthenticationProtocol_MS_CHAPv1 :: RadiusAuthenticationProtocol
pattern RadiusAuthenticationProtocol_MS_CHAPv1 = RadiusAuthenticationProtocol' "MS-CHAPv1"

pattern RadiusAuthenticationProtocol_MS_CHAPv2 :: RadiusAuthenticationProtocol
pattern RadiusAuthenticationProtocol_MS_CHAPv2 = RadiusAuthenticationProtocol' "MS-CHAPv2"

pattern RadiusAuthenticationProtocol_PAP :: RadiusAuthenticationProtocol
pattern RadiusAuthenticationProtocol_PAP = RadiusAuthenticationProtocol' "PAP"

{-# COMPLETE
  RadiusAuthenticationProtocol_CHAP,
  RadiusAuthenticationProtocol_MS_CHAPv1,
  RadiusAuthenticationProtocol_MS_CHAPv2,
  RadiusAuthenticationProtocol_PAP,
  RadiusAuthenticationProtocol'
  #-}
