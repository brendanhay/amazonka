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
-- Module      : Network.AWS.WAF.Types.IPSetDescriptorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.IPSetDescriptorType
  ( IPSetDescriptorType
      ( ..,
        IPSetDescriptorType_IPV4,
        IPSetDescriptorType_IPV6
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype IPSetDescriptorType = IPSetDescriptorType'
  { fromIPSetDescriptorType ::
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

pattern IPSetDescriptorType_IPV4 :: IPSetDescriptorType
pattern IPSetDescriptorType_IPV4 = IPSetDescriptorType' "IPV4"

pattern IPSetDescriptorType_IPV6 :: IPSetDescriptorType
pattern IPSetDescriptorType_IPV6 = IPSetDescriptorType' "IPV6"

{-# COMPLETE
  IPSetDescriptorType_IPV4,
  IPSetDescriptorType_IPV6,
  IPSetDescriptorType'
  #-}
