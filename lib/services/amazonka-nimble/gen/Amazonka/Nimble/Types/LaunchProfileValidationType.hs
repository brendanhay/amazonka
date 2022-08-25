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
-- Module      : Amazonka.Nimble.Types.LaunchProfileValidationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileValidationType
  ( LaunchProfileValidationType
      ( ..,
        LaunchProfileValidationType_VALIDATE_ACTIVE_DIRECTORY_STUDIO_COMPONENT,
        LaunchProfileValidationType_VALIDATE_NETWORK_ACL_ASSOCIATION,
        LaunchProfileValidationType_VALIDATE_SECURITY_GROUP_ASSOCIATION,
        LaunchProfileValidationType_VALIDATE_SUBNET_ASSOCIATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LaunchProfileValidationType = LaunchProfileValidationType'
  { fromLaunchProfileValidationType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern LaunchProfileValidationType_VALIDATE_ACTIVE_DIRECTORY_STUDIO_COMPONENT :: LaunchProfileValidationType
pattern LaunchProfileValidationType_VALIDATE_ACTIVE_DIRECTORY_STUDIO_COMPONENT = LaunchProfileValidationType' "VALIDATE_ACTIVE_DIRECTORY_STUDIO_COMPONENT"

pattern LaunchProfileValidationType_VALIDATE_NETWORK_ACL_ASSOCIATION :: LaunchProfileValidationType
pattern LaunchProfileValidationType_VALIDATE_NETWORK_ACL_ASSOCIATION = LaunchProfileValidationType' "VALIDATE_NETWORK_ACL_ASSOCIATION"

pattern LaunchProfileValidationType_VALIDATE_SECURITY_GROUP_ASSOCIATION :: LaunchProfileValidationType
pattern LaunchProfileValidationType_VALIDATE_SECURITY_GROUP_ASSOCIATION = LaunchProfileValidationType' "VALIDATE_SECURITY_GROUP_ASSOCIATION"

pattern LaunchProfileValidationType_VALIDATE_SUBNET_ASSOCIATION :: LaunchProfileValidationType
pattern LaunchProfileValidationType_VALIDATE_SUBNET_ASSOCIATION = LaunchProfileValidationType' "VALIDATE_SUBNET_ASSOCIATION"

{-# COMPLETE
  LaunchProfileValidationType_VALIDATE_ACTIVE_DIRECTORY_STUDIO_COMPONENT,
  LaunchProfileValidationType_VALIDATE_NETWORK_ACL_ASSOCIATION,
  LaunchProfileValidationType_VALIDATE_SECURITY_GROUP_ASSOCIATION,
  LaunchProfileValidationType_VALIDATE_SUBNET_ASSOCIATION,
  LaunchProfileValidationType'
  #-}
