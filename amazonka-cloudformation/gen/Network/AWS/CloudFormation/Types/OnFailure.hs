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
-- Module      : Network.AWS.CloudFormation.Types.OnFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.OnFailure
  ( OnFailure
      ( ..,
        OnFailure_DELETE,
        OnFailure_DO_NOTHING,
        OnFailure_ROLLBACK
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OnFailure = OnFailure'
  { fromOnFailure ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
