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
-- Module      : Network.AWS.DMS.Types.AuthMechanismValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AuthMechanismValue
  ( AuthMechanismValue
      ( ..,
        AuthMechanismValue_Default,
        AuthMechanismValue_Mongodb_cr,
        AuthMechanismValue_Scram_sha_1
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuthMechanismValue = AuthMechanismValue'
  { fromAuthMechanismValue ::
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

pattern AuthMechanismValue_Default :: AuthMechanismValue
pattern AuthMechanismValue_Default = AuthMechanismValue' "default"

pattern AuthMechanismValue_Mongodb_cr :: AuthMechanismValue
pattern AuthMechanismValue_Mongodb_cr = AuthMechanismValue' "mongodb_cr"

pattern AuthMechanismValue_Scram_sha_1 :: AuthMechanismValue
pattern AuthMechanismValue_Scram_sha_1 = AuthMechanismValue' "scram_sha_1"

{-# COMPLETE
  AuthMechanismValue_Default,
  AuthMechanismValue_Mongodb_cr,
  AuthMechanismValue_Scram_sha_1,
  AuthMechanismValue'
  #-}
