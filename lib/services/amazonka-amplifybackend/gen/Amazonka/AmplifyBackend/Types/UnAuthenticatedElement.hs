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
-- Module      : Amazonka.AmplifyBackend.Types.UnAuthenticatedElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UnAuthenticatedElement
  ( UnAuthenticatedElement
      ( ..,
        UnAuthenticatedElement_CREATE_AND_UPDATE,
        UnAuthenticatedElement_DELETE,
        UnAuthenticatedElement_READ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnAuthenticatedElement = UnAuthenticatedElement'
  { fromUnAuthenticatedElement ::
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

pattern UnAuthenticatedElement_CREATE_AND_UPDATE :: UnAuthenticatedElement
pattern UnAuthenticatedElement_CREATE_AND_UPDATE = UnAuthenticatedElement' "CREATE_AND_UPDATE"

pattern UnAuthenticatedElement_DELETE :: UnAuthenticatedElement
pattern UnAuthenticatedElement_DELETE = UnAuthenticatedElement' "DELETE"

pattern UnAuthenticatedElement_READ :: UnAuthenticatedElement
pattern UnAuthenticatedElement_READ = UnAuthenticatedElement' "READ"

{-# COMPLETE
  UnAuthenticatedElement_CREATE_AND_UPDATE,
  UnAuthenticatedElement_DELETE,
  UnAuthenticatedElement_READ,
  UnAuthenticatedElement'
  #-}
