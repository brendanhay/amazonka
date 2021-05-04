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
-- Module      : Network.AWS.DMS.Types.DmsSslModeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DmsSslModeValue
  ( DmsSslModeValue
      ( ..,
        DmsSslModeValue_None,
        DmsSslModeValue_Require,
        DmsSslModeValue_Verify_ca,
        DmsSslModeValue_Verify_full
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DmsSslModeValue = DmsSslModeValue'
  { fromDmsSslModeValue ::
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

pattern DmsSslModeValue_None :: DmsSslModeValue
pattern DmsSslModeValue_None = DmsSslModeValue' "none"

pattern DmsSslModeValue_Require :: DmsSslModeValue
pattern DmsSslModeValue_Require = DmsSslModeValue' "require"

pattern DmsSslModeValue_Verify_ca :: DmsSslModeValue
pattern DmsSslModeValue_Verify_ca = DmsSslModeValue' "verify-ca"

pattern DmsSslModeValue_Verify_full :: DmsSslModeValue
pattern DmsSslModeValue_Verify_full = DmsSslModeValue' "verify-full"

{-# COMPLETE
  DmsSslModeValue_None,
  DmsSslModeValue_Require,
  DmsSslModeValue_Verify_ca,
  DmsSslModeValue_Verify_full,
  DmsSslModeValue'
  #-}
