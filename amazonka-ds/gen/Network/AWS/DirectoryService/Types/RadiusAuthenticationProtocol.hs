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
-- Module      : Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
  ( RadiusAuthenticationProtocol
      ( ..,
        RadiusAuthenticationProtocol_CHAP,
        RadiusAuthenticationProtocol_MS_CHAPv1,
        RadiusAuthenticationProtocol_MS_CHAPv2,
        RadiusAuthenticationProtocol_PAP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RadiusAuthenticationProtocol = RadiusAuthenticationProtocol'
  { fromRadiusAuthenticationProtocol ::
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
