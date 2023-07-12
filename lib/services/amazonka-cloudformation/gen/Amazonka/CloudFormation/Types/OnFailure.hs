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
-- Module      : Amazonka.CloudFormation.Types.OnFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.OnFailure
  ( OnFailure
      ( ..,
        OnFailure_DELETE,
        OnFailure_DO_NOTHING,
        OnFailure_ROLLBACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OnFailure = OnFailure'
  { fromOnFailure ::
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

pattern OnFailure_DELETE :: OnFailure
pattern OnFailure_DELETE = OnFailure' "DELETE"

pattern OnFailure_DO_NOTHING :: OnFailure
pattern OnFailure_DO_NOTHING = OnFailure' "DO_NOTHING"

pattern OnFailure_ROLLBACK :: OnFailure
pattern OnFailure_ROLLBACK = OnFailure' "ROLLBACK"

{-# COMPLETE
  OnFailure_DELETE,
  OnFailure_DO_NOTHING,
  OnFailure_ROLLBACK,
  OnFailure'
  #-}
