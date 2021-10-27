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
-- Module      : Network.AWS.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
  ( AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
      ( ..,
        AwsS3BucketNotificationConfigurationS3KeyFilterRuleName_Prefix,
        AwsS3BucketNotificationConfigurationS3KeyFilterRuleName_Suffix
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AwsS3BucketNotificationConfigurationS3KeyFilterRuleName = AwsS3BucketNotificationConfigurationS3KeyFilterRuleName'
  { fromAwsS3BucketNotificationConfigurationS3KeyFilterRuleName ::
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

pattern AwsS3BucketNotificationConfigurationS3KeyFilterRuleName_Prefix :: AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
pattern AwsS3BucketNotificationConfigurationS3KeyFilterRuleName_Prefix = AwsS3BucketNotificationConfigurationS3KeyFilterRuleName' "Prefix"

pattern AwsS3BucketNotificationConfigurationS3KeyFilterRuleName_Suffix :: AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
pattern AwsS3BucketNotificationConfigurationS3KeyFilterRuleName_Suffix = AwsS3BucketNotificationConfigurationS3KeyFilterRuleName' "Suffix"

{-# COMPLETE
  AwsS3BucketNotificationConfigurationS3KeyFilterRuleName_Prefix,
  AwsS3BucketNotificationConfigurationS3KeyFilterRuleName_Suffix,
  AwsS3BucketNotificationConfigurationS3KeyFilterRuleName'
  #-}
