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
-- Module      : Network.AWS.Greengrass.Types.UpdateTargetsArchitecture
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.UpdateTargetsArchitecture
  ( UpdateTargetsArchitecture
      ( ..,
        UpdateTargetsArchitecture_Aarch64,
        UpdateTargetsArchitecture_Armv6l,
        UpdateTargetsArchitecture_Armv7l,
        UpdateTargetsArchitecture_X86_64
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The architecture of the cores which are the targets of an update.
newtype UpdateTargetsArchitecture = UpdateTargetsArchitecture'
  { fromUpdateTargetsArchitecture ::
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

pattern UpdateTargetsArchitecture_Aarch64 :: UpdateTargetsArchitecture
pattern UpdateTargetsArchitecture_Aarch64 = UpdateTargetsArchitecture' "aarch64"

pattern UpdateTargetsArchitecture_Armv6l :: UpdateTargetsArchitecture
pattern UpdateTargetsArchitecture_Armv6l = UpdateTargetsArchitecture' "armv6l"

pattern UpdateTargetsArchitecture_Armv7l :: UpdateTargetsArchitecture
pattern UpdateTargetsArchitecture_Armv7l = UpdateTargetsArchitecture' "armv7l"

pattern UpdateTargetsArchitecture_X86_64 :: UpdateTargetsArchitecture
pattern UpdateTargetsArchitecture_X86_64 = UpdateTargetsArchitecture' "x86_64"

{-# COMPLETE
  UpdateTargetsArchitecture_Aarch64,
  UpdateTargetsArchitecture_Armv6l,
  UpdateTargetsArchitecture_Armv7l,
  UpdateTargetsArchitecture_X86_64,
  UpdateTargetsArchitecture'
  #-}
