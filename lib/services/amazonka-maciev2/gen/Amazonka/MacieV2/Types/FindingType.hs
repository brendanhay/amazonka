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
-- Module      : Amazonka.MacieV2.Types.FindingType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingType
  ( FindingType
      ( ..,
        FindingType_Policy_IAMUser_S3BlockPublicAccessDisabled,
        FindingType_Policy_IAMUser_S3BucketEncryptionDisabled,
        FindingType_Policy_IAMUser_S3BucketPublic,
        FindingType_Policy_IAMUser_S3BucketReplicatedExternally,
        FindingType_Policy_IAMUser_S3BucketSharedExternally,
        FindingType_SensitiveData_S3Object_Credentials,
        FindingType_SensitiveData_S3Object_CustomIdentifier,
        FindingType_SensitiveData_S3Object_Financial,
        FindingType_SensitiveData_S3Object_Multiple,
        FindingType_SensitiveData_S3Object_Personal
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of finding. For details about each type, see
-- <https://docs.aws.amazon.com/macie/latest/user/findings-types.html Types of Amazon Macie findings>
-- in the /Amazon Macie User Guide/. Possible values are:
newtype FindingType = FindingType'
  { fromFindingType ::
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

pattern FindingType_Policy_IAMUser_S3BlockPublicAccessDisabled :: FindingType
pattern FindingType_Policy_IAMUser_S3BlockPublicAccessDisabled = FindingType' "Policy:IAMUser/S3BlockPublicAccessDisabled"

pattern FindingType_Policy_IAMUser_S3BucketEncryptionDisabled :: FindingType
pattern FindingType_Policy_IAMUser_S3BucketEncryptionDisabled = FindingType' "Policy:IAMUser/S3BucketEncryptionDisabled"

pattern FindingType_Policy_IAMUser_S3BucketPublic :: FindingType
pattern FindingType_Policy_IAMUser_S3BucketPublic = FindingType' "Policy:IAMUser/S3BucketPublic"

pattern FindingType_Policy_IAMUser_S3BucketReplicatedExternally :: FindingType
pattern FindingType_Policy_IAMUser_S3BucketReplicatedExternally = FindingType' "Policy:IAMUser/S3BucketReplicatedExternally"

pattern FindingType_Policy_IAMUser_S3BucketSharedExternally :: FindingType
pattern FindingType_Policy_IAMUser_S3BucketSharedExternally = FindingType' "Policy:IAMUser/S3BucketSharedExternally"

pattern FindingType_SensitiveData_S3Object_Credentials :: FindingType
pattern FindingType_SensitiveData_S3Object_Credentials = FindingType' "SensitiveData:S3Object/Credentials"

pattern FindingType_SensitiveData_S3Object_CustomIdentifier :: FindingType
pattern FindingType_SensitiveData_S3Object_CustomIdentifier = FindingType' "SensitiveData:S3Object/CustomIdentifier"

pattern FindingType_SensitiveData_S3Object_Financial :: FindingType
pattern FindingType_SensitiveData_S3Object_Financial = FindingType' "SensitiveData:S3Object/Financial"

pattern FindingType_SensitiveData_S3Object_Multiple :: FindingType
pattern FindingType_SensitiveData_S3Object_Multiple = FindingType' "SensitiveData:S3Object/Multiple"

pattern FindingType_SensitiveData_S3Object_Personal :: FindingType
pattern FindingType_SensitiveData_S3Object_Personal = FindingType' "SensitiveData:S3Object/Personal"

{-# COMPLETE
  FindingType_Policy_IAMUser_S3BlockPublicAccessDisabled,
  FindingType_Policy_IAMUser_S3BucketEncryptionDisabled,
  FindingType_Policy_IAMUser_S3BucketPublic,
  FindingType_Policy_IAMUser_S3BucketReplicatedExternally,
  FindingType_Policy_IAMUser_S3BucketSharedExternally,
  FindingType_SensitiveData_S3Object_Credentials,
  FindingType_SensitiveData_S3Object_CustomIdentifier,
  FindingType_SensitiveData_S3Object_Financial,
  FindingType_SensitiveData_S3Object_Multiple,
  FindingType_SensitiveData_S3Object_Personal,
  FindingType'
  #-}
