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
-- Module      : Amazonka.DataSync.Types.SmbVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.SmbVersion
  ( SmbVersion
      ( ..,
        SmbVersion_AUTOMATIC,
        SmbVersion_SMB2,
        SmbVersion_SMB3
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SmbVersion = SmbVersion'
  { fromSmbVersion ::
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

pattern SmbVersion_AUTOMATIC :: SmbVersion
pattern SmbVersion_AUTOMATIC = SmbVersion' "AUTOMATIC"

pattern SmbVersion_SMB2 :: SmbVersion
pattern SmbVersion_SMB2 = SmbVersion' "SMB2"

pattern SmbVersion_SMB3 :: SmbVersion
pattern SmbVersion_SMB3 = SmbVersion' "SMB3"

{-# COMPLETE
  SmbVersion_AUTOMATIC,
  SmbVersion_SMB2,
  SmbVersion_SMB3,
  SmbVersion'
  #-}
