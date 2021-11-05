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
-- Module      : Amazonka.Signer.Types.SigningProfileStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningProfileStatus
  ( SigningProfileStatus
      ( ..,
        SigningProfileStatus_Active,
        SigningProfileStatus_Canceled,
        SigningProfileStatus_Revoked
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SigningProfileStatus = SigningProfileStatus'
  { fromSigningProfileStatus ::
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

pattern SigningProfileStatus_Active :: SigningProfileStatus
pattern SigningProfileStatus_Active = SigningProfileStatus' "Active"

pattern SigningProfileStatus_Canceled :: SigningProfileStatus
pattern SigningProfileStatus_Canceled = SigningProfileStatus' "Canceled"

pattern SigningProfileStatus_Revoked :: SigningProfileStatus
pattern SigningProfileStatus_Revoked = SigningProfileStatus' "Revoked"

{-# COMPLETE
  SigningProfileStatus_Active,
  SigningProfileStatus_Canceled,
  SigningProfileStatus_Revoked,
  SigningProfileStatus'
  #-}
