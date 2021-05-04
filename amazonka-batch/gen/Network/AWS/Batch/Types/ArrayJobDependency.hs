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
-- Module      : Network.AWS.Batch.Types.ArrayJobDependency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayJobDependency
  ( ArrayJobDependency
      ( ..,
        ArrayJobDependency_N_TO_N,
        ArrayJobDependency_SEQUENTIAL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ArrayJobDependency = ArrayJobDependency'
  { fromArrayJobDependency ::
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

pattern ArrayJobDependency_N_TO_N :: ArrayJobDependency
pattern ArrayJobDependency_N_TO_N = ArrayJobDependency' "N_TO_N"

pattern ArrayJobDependency_SEQUENTIAL :: ArrayJobDependency
pattern ArrayJobDependency_SEQUENTIAL = ArrayJobDependency' "SEQUENTIAL"

{-# COMPLETE
  ArrayJobDependency_N_TO_N,
  ArrayJobDependency_SEQUENTIAL,
  ArrayJobDependency'
  #-}
