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
-- Module      : Amazonka.Amplify.Types.RepositoryCloneMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.RepositoryCloneMethod
  ( RepositoryCloneMethod
      ( ..,
        RepositoryCloneMethod_SIGV4,
        RepositoryCloneMethod_SSH,
        RepositoryCloneMethod_TOKEN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RepositoryCloneMethod = RepositoryCloneMethod'
  { fromRepositoryCloneMethod ::
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

pattern RepositoryCloneMethod_SIGV4 :: RepositoryCloneMethod
pattern RepositoryCloneMethod_SIGV4 = RepositoryCloneMethod' "SIGV4"

pattern RepositoryCloneMethod_SSH :: RepositoryCloneMethod
pattern RepositoryCloneMethod_SSH = RepositoryCloneMethod' "SSH"

pattern RepositoryCloneMethod_TOKEN :: RepositoryCloneMethod
pattern RepositoryCloneMethod_TOKEN = RepositoryCloneMethod' "TOKEN"

{-# COMPLETE
  RepositoryCloneMethod_SIGV4,
  RepositoryCloneMethod_SSH,
  RepositoryCloneMethod_TOKEN,
  RepositoryCloneMethod'
  #-}
