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
-- Module      : Network.AWS.EC2.Types.EbsNvmeSupport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EbsNvmeSupport
  ( EbsNvmeSupport
      ( ..,
        EbsNvmeSupport_Required,
        EbsNvmeSupport_Supported,
        EbsNvmeSupport_Unsupported
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype EbsNvmeSupport = EbsNvmeSupport'
  { fromEbsNvmeSupport ::
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

pattern EbsNvmeSupport_Required :: EbsNvmeSupport
pattern EbsNvmeSupport_Required = EbsNvmeSupport' "required"

pattern EbsNvmeSupport_Supported :: EbsNvmeSupport
pattern EbsNvmeSupport_Supported = EbsNvmeSupport' "supported"

pattern EbsNvmeSupport_Unsupported :: EbsNvmeSupport
pattern EbsNvmeSupport_Unsupported = EbsNvmeSupport' "unsupported"

{-# COMPLETE
  EbsNvmeSupport_Required,
  EbsNvmeSupport_Supported,
  EbsNvmeSupport_Unsupported,
  EbsNvmeSupport'
  #-}
