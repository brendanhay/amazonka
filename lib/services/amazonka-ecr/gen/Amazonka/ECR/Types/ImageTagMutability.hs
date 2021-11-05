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
-- Module      : Amazonka.ECR.Types.ImageTagMutability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageTagMutability
  ( ImageTagMutability
      ( ..,
        ImageTagMutability_IMMUTABLE,
        ImageTagMutability_MUTABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ImageTagMutability = ImageTagMutability'
  { fromImageTagMutability ::
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

pattern ImageTagMutability_IMMUTABLE :: ImageTagMutability
pattern ImageTagMutability_IMMUTABLE = ImageTagMutability' "IMMUTABLE"

pattern ImageTagMutability_MUTABLE :: ImageTagMutability
pattern ImageTagMutability_MUTABLE = ImageTagMutability' "MUTABLE"

{-# COMPLETE
  ImageTagMutability_IMMUTABLE,
  ImageTagMutability_MUTABLE,
  ImageTagMutability'
  #-}
