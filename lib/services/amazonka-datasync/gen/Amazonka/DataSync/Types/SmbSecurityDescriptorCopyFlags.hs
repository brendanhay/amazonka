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
-- Module      : Amazonka.DataSync.Types.SmbSecurityDescriptorCopyFlags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.SmbSecurityDescriptorCopyFlags
  ( SmbSecurityDescriptorCopyFlags
      ( ..,
        SmbSecurityDescriptorCopyFlags_NONE,
        SmbSecurityDescriptorCopyFlags_OWNER_DACL,
        SmbSecurityDescriptorCopyFlags_OWNER_DACL_SACL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SmbSecurityDescriptorCopyFlags = SmbSecurityDescriptorCopyFlags'
  { fromSmbSecurityDescriptorCopyFlags ::
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

pattern SmbSecurityDescriptorCopyFlags_NONE :: SmbSecurityDescriptorCopyFlags
pattern SmbSecurityDescriptorCopyFlags_NONE = SmbSecurityDescriptorCopyFlags' "NONE"

pattern SmbSecurityDescriptorCopyFlags_OWNER_DACL :: SmbSecurityDescriptorCopyFlags
pattern SmbSecurityDescriptorCopyFlags_OWNER_DACL = SmbSecurityDescriptorCopyFlags' "OWNER_DACL"

pattern SmbSecurityDescriptorCopyFlags_OWNER_DACL_SACL :: SmbSecurityDescriptorCopyFlags
pattern SmbSecurityDescriptorCopyFlags_OWNER_DACL_SACL = SmbSecurityDescriptorCopyFlags' "OWNER_DACL_SACL"

{-# COMPLETE
  SmbSecurityDescriptorCopyFlags_NONE,
  SmbSecurityDescriptorCopyFlags_OWNER_DACL,
  SmbSecurityDescriptorCopyFlags_OWNER_DACL_SACL,
  SmbSecurityDescriptorCopyFlags'
  #-}
