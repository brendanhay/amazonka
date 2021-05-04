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
-- Module      : Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
  ( LastUpdateStatusReasonCode
      ( ..,
        LastUpdateStatusReasonCode_EniLimitExceeded,
        LastUpdateStatusReasonCode_ImageAccessDenied,
        LastUpdateStatusReasonCode_ImageDeleted,
        LastUpdateStatusReasonCode_InsufficientRolePermissions,
        LastUpdateStatusReasonCode_InternalError,
        LastUpdateStatusReasonCode_InvalidConfiguration,
        LastUpdateStatusReasonCode_InvalidImage,
        LastUpdateStatusReasonCode_InvalidSecurityGroup,
        LastUpdateStatusReasonCode_InvalidSubnet,
        LastUpdateStatusReasonCode_SubnetOutOfIPAddresses
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LastUpdateStatusReasonCode = LastUpdateStatusReasonCode'
  { fromLastUpdateStatusReasonCode ::
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

pattern LastUpdateStatusReasonCode_EniLimitExceeded :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_EniLimitExceeded = LastUpdateStatusReasonCode' "EniLimitExceeded"

pattern LastUpdateStatusReasonCode_ImageAccessDenied :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_ImageAccessDenied = LastUpdateStatusReasonCode' "ImageAccessDenied"

pattern LastUpdateStatusReasonCode_ImageDeleted :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_ImageDeleted = LastUpdateStatusReasonCode' "ImageDeleted"

pattern LastUpdateStatusReasonCode_InsufficientRolePermissions :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InsufficientRolePermissions = LastUpdateStatusReasonCode' "InsufficientRolePermissions"

pattern LastUpdateStatusReasonCode_InternalError :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InternalError = LastUpdateStatusReasonCode' "InternalError"

pattern LastUpdateStatusReasonCode_InvalidConfiguration :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidConfiguration = LastUpdateStatusReasonCode' "InvalidConfiguration"

pattern LastUpdateStatusReasonCode_InvalidImage :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidImage = LastUpdateStatusReasonCode' "InvalidImage"

pattern LastUpdateStatusReasonCode_InvalidSecurityGroup :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidSecurityGroup = LastUpdateStatusReasonCode' "InvalidSecurityGroup"

pattern LastUpdateStatusReasonCode_InvalidSubnet :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidSubnet = LastUpdateStatusReasonCode' "InvalidSubnet"

pattern LastUpdateStatusReasonCode_SubnetOutOfIPAddresses :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_SubnetOutOfIPAddresses = LastUpdateStatusReasonCode' "SubnetOutOfIPAddresses"

{-# COMPLETE
  LastUpdateStatusReasonCode_EniLimitExceeded,
  LastUpdateStatusReasonCode_ImageAccessDenied,
  LastUpdateStatusReasonCode_ImageDeleted,
  LastUpdateStatusReasonCode_InsufficientRolePermissions,
  LastUpdateStatusReasonCode_InternalError,
  LastUpdateStatusReasonCode_InvalidConfiguration,
  LastUpdateStatusReasonCode_InvalidImage,
  LastUpdateStatusReasonCode_InvalidSecurityGroup,
  LastUpdateStatusReasonCode_InvalidSubnet,
  LastUpdateStatusReasonCode_SubnetOutOfIPAddresses,
  LastUpdateStatusReasonCode'
  #-}
