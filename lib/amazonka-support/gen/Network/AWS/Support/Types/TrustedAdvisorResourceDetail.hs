{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorResourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorResourceDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a resource identified by a Trusted Advisor check.
--
--
--
-- /See:/ 'trustedAdvisorResourceDetail' smart constructor.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail'
  { _tardIsSuppressed ::
      !(Maybe Bool),
    _tardRegion :: !(Maybe Text),
    _tardStatus :: !Text,
    _tardResourceId :: !Text,
    _tardMetadata :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedAdvisorResourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tardIsSuppressed' - Specifies whether the AWS resource was ignored by Trusted Advisor because it was marked as suppressed by the user.
--
-- * 'tardRegion' - The AWS region in which the identified resource is located.
--
-- * 'tardStatus' - The status code for the resource identified in the Trusted Advisor check.
--
-- * 'tardResourceId' - The unique identifier for the identified resource.
--
-- * 'tardMetadata' - Additional information about the identified resource. The exact metadata and its order can be obtained by inspecting the 'TrustedAdvisorCheckDescription' object returned by the call to 'DescribeTrustedAdvisorChecks' . __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data.
trustedAdvisorResourceDetail ::
  -- | 'tardStatus'
  Text ->
  -- | 'tardResourceId'
  Text ->
  TrustedAdvisorResourceDetail
trustedAdvisorResourceDetail pStatus_ pResourceId_ =
  TrustedAdvisorResourceDetail'
    { _tardIsSuppressed = Nothing,
      _tardRegion = Nothing,
      _tardStatus = pStatus_,
      _tardResourceId = pResourceId_,
      _tardMetadata = mempty
    }

-- | Specifies whether the AWS resource was ignored by Trusted Advisor because it was marked as suppressed by the user.
tardIsSuppressed :: Lens' TrustedAdvisorResourceDetail (Maybe Bool)
tardIsSuppressed = lens _tardIsSuppressed (\s a -> s {_tardIsSuppressed = a})

-- | The AWS region in which the identified resource is located.
tardRegion :: Lens' TrustedAdvisorResourceDetail (Maybe Text)
tardRegion = lens _tardRegion (\s a -> s {_tardRegion = a})

-- | The status code for the resource identified in the Trusted Advisor check.
tardStatus :: Lens' TrustedAdvisorResourceDetail Text
tardStatus = lens _tardStatus (\s a -> s {_tardStatus = a})

-- | The unique identifier for the identified resource.
tardResourceId :: Lens' TrustedAdvisorResourceDetail Text
tardResourceId = lens _tardResourceId (\s a -> s {_tardResourceId = a})

-- | Additional information about the identified resource. The exact metadata and its order can be obtained by inspecting the 'TrustedAdvisorCheckDescription' object returned by the call to 'DescribeTrustedAdvisorChecks' . __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data.
tardMetadata :: Lens' TrustedAdvisorResourceDetail [Text]
tardMetadata = lens _tardMetadata (\s a -> s {_tardMetadata = a}) . _Coerce

instance FromJSON TrustedAdvisorResourceDetail where
  parseJSON =
    withObject
      "TrustedAdvisorResourceDetail"
      ( \x ->
          TrustedAdvisorResourceDetail'
            <$> (x .:? "isSuppressed")
            <*> (x .:? "region")
            <*> (x .: "status")
            <*> (x .: "resourceId")
            <*> (x .:? "metadata" .!= mempty)
      )

instance Hashable TrustedAdvisorResourceDetail

instance NFData TrustedAdvisorResourceDetail
