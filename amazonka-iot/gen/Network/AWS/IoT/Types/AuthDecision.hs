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
-- Module      : Network.AWS.IoT.Types.AuthDecision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthDecision
  ( AuthDecision
      ( ..,
        AuthDecision_ALLOWED,
        AuthDecision_EXPLICIT_DENY,
        AuthDecision_IMPLICIT_DENY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuthDecision = AuthDecision'
  { fromAuthDecision ::
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

pattern AuthDecision_ALLOWED :: AuthDecision
pattern AuthDecision_ALLOWED = AuthDecision' "ALLOWED"

pattern AuthDecision_EXPLICIT_DENY :: AuthDecision
pattern AuthDecision_EXPLICIT_DENY = AuthDecision' "EXPLICIT_DENY"

pattern AuthDecision_IMPLICIT_DENY :: AuthDecision
pattern AuthDecision_IMPLICIT_DENY = AuthDecision' "IMPLICIT_DENY"

{-# COMPLETE
  AuthDecision_ALLOWED,
  AuthDecision_EXPLICIT_DENY,
  AuthDecision_IMPLICIT_DENY,
  AuthDecision'
  #-}
