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
-- Module      : Amazonka.Transfer.Types.SftpAuthenticationMethods
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.SftpAuthenticationMethods
  ( SftpAuthenticationMethods
      ( ..,
        SftpAuthenticationMethods_PASSWORD,
        SftpAuthenticationMethods_PUBLIC_KEY,
        SftpAuthenticationMethods_PUBLIC_KEY_AND_PASSWORD,
        SftpAuthenticationMethods_PUBLIC_KEY_OR_PASSWORD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SftpAuthenticationMethods = SftpAuthenticationMethods'
  { fromSftpAuthenticationMethods ::
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

pattern SftpAuthenticationMethods_PASSWORD :: SftpAuthenticationMethods
pattern SftpAuthenticationMethods_PASSWORD = SftpAuthenticationMethods' "PASSWORD"

pattern SftpAuthenticationMethods_PUBLIC_KEY :: SftpAuthenticationMethods
pattern SftpAuthenticationMethods_PUBLIC_KEY = SftpAuthenticationMethods' "PUBLIC_KEY"

pattern SftpAuthenticationMethods_PUBLIC_KEY_AND_PASSWORD :: SftpAuthenticationMethods
pattern SftpAuthenticationMethods_PUBLIC_KEY_AND_PASSWORD = SftpAuthenticationMethods' "PUBLIC_KEY_AND_PASSWORD"

pattern SftpAuthenticationMethods_PUBLIC_KEY_OR_PASSWORD :: SftpAuthenticationMethods
pattern SftpAuthenticationMethods_PUBLIC_KEY_OR_PASSWORD = SftpAuthenticationMethods' "PUBLIC_KEY_OR_PASSWORD"

{-# COMPLETE
  SftpAuthenticationMethods_PASSWORD,
  SftpAuthenticationMethods_PUBLIC_KEY,
  SftpAuthenticationMethods_PUBLIC_KEY_AND_PASSWORD,
  SftpAuthenticationMethods_PUBLIC_KEY_OR_PASSWORD,
  SftpAuthenticationMethods'
  #-}
