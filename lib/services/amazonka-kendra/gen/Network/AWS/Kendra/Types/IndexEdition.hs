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
-- Module      : Network.AWS.Kendra.Types.IndexEdition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.IndexEdition
  ( IndexEdition
      ( ..,
        IndexEdition_DEVELOPER_EDITION,
        IndexEdition_ENTERPRISE_EDITION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype IndexEdition = IndexEdition'
  { fromIndexEdition ::
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

pattern IndexEdition_DEVELOPER_EDITION :: IndexEdition
pattern IndexEdition_DEVELOPER_EDITION = IndexEdition' "DEVELOPER_EDITION"

pattern IndexEdition_ENTERPRISE_EDITION :: IndexEdition
pattern IndexEdition_ENTERPRISE_EDITION = IndexEdition' "ENTERPRISE_EDITION"

{-# COMPLETE
  IndexEdition_DEVELOPER_EDITION,
  IndexEdition_ENTERPRISE_EDITION,
  IndexEdition'
  #-}
