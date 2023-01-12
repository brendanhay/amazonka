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
-- Module      : Amazonka.DataSync.Types.HdfsAuthenticationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.HdfsAuthenticationType
  ( HdfsAuthenticationType
      ( ..,
        HdfsAuthenticationType_KERBEROS,
        HdfsAuthenticationType_SIMPLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HdfsAuthenticationType = HdfsAuthenticationType'
  { fromHdfsAuthenticationType ::
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

pattern HdfsAuthenticationType_KERBEROS :: HdfsAuthenticationType
pattern HdfsAuthenticationType_KERBEROS = HdfsAuthenticationType' "KERBEROS"

pattern HdfsAuthenticationType_SIMPLE :: HdfsAuthenticationType
pattern HdfsAuthenticationType_SIMPLE = HdfsAuthenticationType' "SIMPLE"

{-# COMPLETE
  HdfsAuthenticationType_KERBEROS,
  HdfsAuthenticationType_SIMPLE,
  HdfsAuthenticationType'
  #-}
