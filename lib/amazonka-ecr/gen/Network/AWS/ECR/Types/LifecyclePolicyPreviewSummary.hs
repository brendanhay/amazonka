{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary of the lifecycle policy preview request.
--
--
--
-- /See:/ 'lifecyclePolicyPreviewSummary' smart constructor.
newtype LifecyclePolicyPreviewSummary = LifecyclePolicyPreviewSummary'
  { _lppsExpiringImageTotalCount ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecyclePolicyPreviewSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lppsExpiringImageTotalCount' - The number of expiring images.
lifecyclePolicyPreviewSummary ::
  LifecyclePolicyPreviewSummary
lifecyclePolicyPreviewSummary =
  LifecyclePolicyPreviewSummary'
    { _lppsExpiringImageTotalCount =
        Nothing
    }

-- | The number of expiring images.
lppsExpiringImageTotalCount :: Lens' LifecyclePolicyPreviewSummary (Maybe Natural)
lppsExpiringImageTotalCount = lens _lppsExpiringImageTotalCount (\s a -> s {_lppsExpiringImageTotalCount = a}) . mapping _Nat

instance FromJSON LifecyclePolicyPreviewSummary where
  parseJSON =
    withObject
      "LifecyclePolicyPreviewSummary"
      ( \x ->
          LifecyclePolicyPreviewSummary'
            <$> (x .:? "expiringImageTotalCount")
      )

instance Hashable LifecyclePolicyPreviewSummary

instance NFData LifecyclePolicyPreviewSummary
