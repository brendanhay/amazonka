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
-- Module      : Amazonka.Kendra.Types.IndexEdition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.IndexEdition
  ( IndexEdition
      ( ..,
        IndexEdition_DEVELOPER_EDITION,
        IndexEdition_ENTERPRISE_EDITION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IndexEdition = IndexEdition'
  { fromIndexEdition ::
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

pattern IndexEdition_DEVELOPER_EDITION :: IndexEdition
pattern IndexEdition_DEVELOPER_EDITION = IndexEdition' "DEVELOPER_EDITION"

pattern IndexEdition_ENTERPRISE_EDITION :: IndexEdition
pattern IndexEdition_ENTERPRISE_EDITION = IndexEdition' "ENTERPRISE_EDITION"

{-# COMPLETE
  IndexEdition_DEVELOPER_EDITION,
  IndexEdition_ENTERPRISE_EDITION,
  IndexEdition'
  #-}
